module Elm.Parser.Expression exposing (expression)

import Elm.Parser.Layout as Layout
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    literalExpression

                "(" ->
                    tupledExpressionIfNecessaryFollowedByRecordAccess

                "[" ->
                    listOrGlslExpression

                "{" ->
                    recordExpressionFollowedByRecordAccess

                "c" ->
                    caseOrUnqualifiedReferenceExpression

                "\\" ->
                    lambdaExpression

                "l" ->
                    letOrUnqualifiedReferenceExpression

                "i" ->
                    ifOrUnqualifiedReferenceExpression

                "." ->
                    recordAccessFunctionExpression

                "-" ->
                    negationOperation

                "'" ->
                    charLiteralExpression

                _ ->
                    referenceOrNumberExpression
        )


caseOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
caseOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        caseExpression
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


letOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
letOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        letExpression
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


ifOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
ifOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        ifBlockExpression
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


referenceOrNumberExpression : Parser (WithComments (Node Expression))
referenceOrNumberExpression =
    ParserFast.oneOf3
        qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
        numberExpression


followedByMultiRecordAccess : Parser (WithComments (Node Expression)) -> Parser (WithComments (Node Expression))
followedByMultiRecordAccess beforeRecordAccesses =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (ParserFast.symbolFollowedBy "." Tokens.functionNameNode)
        beforeRecordAccesses
        (\((Node fieldRange _) as fieldNode) ( leftComments, leftNode ) ->
            let
                (Node leftRange _) =
                    leftNode
            in
            ( leftComments
            , Node { start = leftRange.start, end = fieldRange.end }
                (Expression.RecordAccess leftNode fieldNode)
            )
        )
        Basics.identity


precedence1ApR : InfixOperatorInfo
precedence1ApR =
    infixLeft 1 "|>"


precedence1ApL : InfixOperatorInfo
precedence1ApL =
    infixRight 1 "<|"


precedence2Or : InfixOperatorInfo
precedence2Or =
    infixRight 2 "||"


precedence3And : InfixOperatorInfo
precedence3And =
    infixRight 3 "&&"


precedence4Eq : InfixOperatorInfo
precedence4Eq =
    infixNonAssociative 4 "=="


precedence4Neq : InfixOperatorInfo
precedence4Neq =
    infixNonAssociative 4 "/="


precedence4Le : InfixOperatorInfo
precedence4Le =
    infixNonAssociative 4 "<="


precedence4Ge : InfixOperatorInfo
precedence4Ge =
    infixNonAssociative 4 ">="


precedence4Gt : InfixOperatorInfo
precedence4Gt =
    infixNonAssociative 4 ">"


precedence4Lt : InfixOperatorInfo
precedence4Lt =
    infixNonAssociative 4 "<"


precedence5append : InfixOperatorInfo
precedence5append =
    infixRight 5 "++"


precedence5Cons : InfixOperatorInfo
precedence5Cons =
    infixRight 5 "::"


precedence5Keep : InfixOperatorInfo
precedence5Keep =
    infixLeft 5 "|="


precedence6Add : InfixOperatorInfo
precedence6Add =
    infixLeft 6 "+"


precedence6Sub : InfixOperatorInfo
precedence6Sub =
    infixLeft 6 "-"


precedence6Ignore : InfixOperatorInfo
precedence6Ignore =
    infixLeft 6 "|."


precedence7Idiv : InfixOperatorInfo
precedence7Idiv =
    infixLeft 7 "//"


precedence7Mul : InfixOperatorInfo
precedence7Mul =
    infixLeft 7 "*"


precedence7Fdiv : InfixOperatorInfo
precedence7Fdiv =
    infixLeft 7 "/"


precedence7Slash : InfixOperatorInfo
precedence7Slash =
    infixRight 7 "</>"


precedence8QuestionMark : InfixOperatorInfo
precedence8QuestionMark =
    infixLeft 8 "<?>"


precedence8Pow : InfixOperatorInfo
precedence8Pow =
    infixRight 8 "^"


precedence9ComposeR : InfixOperatorInfo
precedence9ComposeR =
    infixRight 9 ">>"


precedence9ComposeL : InfixOperatorInfo
precedence9ComposeL =
    infixLeft 9 "<<"


expression : Parser (WithComments (Node Expression))
expression =
    extendedSubExpressionOptimisticLayout Ok .extensionRight


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithRange
            (\range s ->
                ( Rope.empty
                , Node
                    -- TODO for v8: don't include extra end width (from bug in elm/parser) in range
                    { start = { row = range.start.row, column = range.start.column - 6 }
                    , end = { row = range.end.row, column = range.end.column + 2 }
                    }
                    (GLSLExpression s)
                )
            )
            (ParserFast.loopUntil
                (ParserFast.symbol "|]" ())
                (ParserFast.oneOf2
                    (ParserFast.symbol "|" "|")
                    (ParserFast.while (\c -> c /= '|'))
                )
                ""
                (\extension soFar ->
                    soFar ++ extension ++ ""
                )
                identity
            )
        )


listOrGlslExpression : Parser (WithComments (Node Expression))
listOrGlslExpression =
    ParserFast.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
expressionAfterOpeningSquareBracket =
    ParserFast.oneOf2
        glslExpressionAfterOpeningSquareBracket
        (ParserFast.map2WithRange
            (\range commentsBefore ( elementsComments, elements ) ->
                ( commentsBefore |> Rope.prependTo elementsComments
                , Node
                    { start = { row = range.start.row, column = range.start.column - 1 }
                    , end = range.end
                    }
                    elements
                )
            )
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbol "]" ( Rope.empty, ListExpr [] ))
                (ParserFast.map3
                    (\( headComments, head ) commentsAfterHead ( tailComments, tail ) ->
                        ( headComments
                            |> Rope.prependTo commentsAfterHead
                            |> Rope.prependTo tailComments
                        , ListExpr (head :: tail)
                        )
                    )
                    expression
                    Layout.maybeLayout
                    (ParserWithComments.many
                        (ParserFast.symbolFollowedBy ","
                            (Layout.maybeAroundBothSides expression)
                        )
                    )
                    |> ParserFast.followedBySymbol "]"
                )
            )
        )



-- recordExpression


recordExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
recordExpressionFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map2WithRange
            (\range commentsBefore ( afterCurlyComments, afterCurly ) ->
                ( commentsBefore
                    |> Rope.prependTo afterCurlyComments
                , Node (rangeMoveStartLeftByOneColumn range) afterCurly
                )
            )
            Layout.maybeLayout
            recordContentsCurlyEnd
            |> followedByMultiRecordAccess
        )


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    ParserFast.oneOf2
        (ParserFast.map5
            (\nameNode commentsAfterFunctionName ( afterNameBeforeFieldsComments, afterNameBeforeFields ) ( tailFieldsComments, tailFields ) commentsBeforeClosingCurly ->
                ( commentsAfterFunctionName
                    |> Rope.prependTo afterNameBeforeFieldsComments
                    |> Rope.prependTo tailFieldsComments
                    |> Rope.prependTo commentsBeforeClosingCurly
                , case afterNameBeforeFields of
                    RecordUpdateFirstSetter firstField ->
                        RecordUpdateExpression nameNode (firstField :: tailFields)

                    FieldsFirstValue firstFieldValue ->
                        RecordExpr (Node.combine Tuple.pair nameNode firstFieldValue :: tailFields)
                )
            )
            Tokens.functionNameNode
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map2
                        (\commentsBefore ( setterComments, setter ) ->
                            ( commentsBefore |> Rope.prependTo setterComments
                            , RecordUpdateFirstSetter setter
                            )
                        )
                        Layout.maybeLayout
                        recordSetterNodeWithLayout
                    )
                )
                (ParserFast.symbolFollowedBy "="
                    (ParserFast.map3
                        (\commentsBefore ( resultComments, result ) commentsAfter ->
                            ( commentsBefore
                                |> Rope.prependTo resultComments
                                |> Rope.prependTo commentsAfter
                            , FieldsFirstValue result
                            )
                        )
                        Layout.maybeLayout
                        expression
                        Layout.maybeLayout
                    )
                )
            )
            recordFields
            (Layout.maybeLayout |> ParserFast.followedBySymbol "}")
        )
        (ParserFast.symbol "}" ( Rope.empty, RecordExpr [] ))


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        (ParserFast.symbolFollowedBy ","
            (ParserFast.map2
                (\commentsBefore ( setterComments, setter ) ->
                    ( commentsBefore |> Rope.prependTo setterComments
                    , setter
                    )
                )
                Layout.maybeLayout
                recordSetterNodeWithLayout
            )
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    ParserFast.map5WithRange
        (\range name commentsAfterFunctionName commentsAfterEquals ( valueComments, value ) commentsAfterExpression ->
            ( commentsAfterFunctionName
                |> Rope.prependTo commentsAfterEquals
                |> Rope.prependTo valueComments
                |> Rope.prependTo commentsAfterExpression
            , Node range ( name, value )
            )
        )
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        expression
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove
        Layout.maybeLayout


literalExpression : Parser (WithComments (Node Expression))
literalExpression =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            ( Rope.empty
            , Node range (Literal string)
            )
        )


charLiteralExpression : Parser (WithComments (Node Expression))
charLiteralExpression =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            ( Rope.empty
            , Node range (CharLiteral char)
            )
        )



-- lambda


lambdaExpression : Parser (WithComments (Node Expression))
lambdaExpression =
    ParserFast.symbolFollowedBy "\\"
        (ParserFast.map6WithStartLocation
            (\start commentsAfterBackslash ( firstArgComments, firstArg ) commentsAfterFirstArg ( secondUpArgsComments, secondUpArgs ) commentsAfterArrow ( resultComments, result ) ->
                let
                    (Node expressionRange _) =
                        result
                in
                ( commentsAfterBackslash
                    |> Rope.prependTo firstArgComments
                    |> Rope.prependTo commentsAfterFirstArg
                    |> Rope.prependTo secondUpArgsComments
                    |> Rope.prependTo commentsAfterArrow
                    |> Rope.prependTo resultComments
                , Node
                    { start = { row = start.row, column = start.column - 1 }
                    , end = expressionRange.end
                    }
                    (LambdaExpression
                        { args = firstArg :: secondUpArgs
                        , expression = result
                        }
                    )
                )
            )
            Layout.maybeLayout
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
            (ParserWithComments.until
                (ParserFast.symbol "->" ())
                (ParserFast.map2
                    (\( patternComments, pattern ) commentsAfter ->
                        ( patternComments
                            |> Rope.prependTo commentsAfter
                        , pattern
                        )
                    )
                    Patterns.patternNotDirectlyComposing
                    Layout.maybeLayout
                )
            )
            Layout.maybeLayout
            expression
        )



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    ParserFast.keywordFollowedBy "case"
        (ParserFast.map5WithStartLocation
            (\start commentsAfterCase casedExpressionResult commentsBeforeOf commentsAfterOf casesResult ->
                let
                    ( firstCase, lastToSecondCase ) =
                        casesResult |> Tuple.second
                in
                ( commentsAfterCase
                    |> Rope.prependTo (casedExpressionResult |> Tuple.first)
                    |> Rope.prependTo commentsBeforeOf
                    |> Rope.prependTo commentsAfterOf
                    |> Rope.prependTo (casesResult |> Tuple.first)
                , Node
                    { start = { row = start.row, column = start.column - 4 }
                    , end =
                        case lastToSecondCase of
                            ( _, Node lastCaseExpressionRange _ ) :: _ ->
                                lastCaseExpressionRange.end

                            [] ->
                                let
                                    ( _, Node firstCaseExpressionRange _ ) =
                                        firstCase
                                in
                                firstCaseExpressionRange.end
                    }
                    (CaseExpression
                        { expression = casedExpressionResult |> Tuple.second
                        , cases = firstCase :: List.reverse lastToSecondCase
                        }
                    )
                )
            )
            Layout.maybeLayout
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "of" Layout.maybeLayout)
            (ParserFast.withIndentSetToColumn caseStatements)
        )


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    ParserFast.map5
        (\( firstCasePatternComments, firstCasePattern ) commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight ( firstCaseExpressionComments, firstCaseExpression ) ( lastToSecondCaseComments, lastToSecondCase ) ->
            ( firstCasePatternComments
                |> Rope.prependTo commentsAfterFirstCasePattern
                |> Rope.prependTo commentsAfterFirstCaseArrowRight
                |> Rope.prependTo firstCaseExpressionComments
                |> Rope.prependTo lastToSecondCaseComments
            , ( ( firstCasePattern, firstCaseExpression )
              , lastToSecondCase
              )
            )
        )
        Patterns.pattern
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "->" Layout.maybeLayout)
        expression
        (ParserWithComments.manyWithoutReverse caseStatement)


caseStatement : Parser (WithComments Case)
caseStatement =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map4
            (\( patternComments, pattern ) commentsBeforeArrowRight commentsAfterArrowRight ( exprComments, expr ) ->
                ( patternComments
                    |> Rope.prependTo commentsBeforeArrowRight
                    |> Rope.prependTo commentsAfterArrowRight
                    |> Rope.prependTo exprComments
                , ( pattern, expr )
                )
            )
            Patterns.pattern
            Layout.maybeLayout
            (ParserFast.symbolFollowedBy "->" Layout.maybeLayout)
            expression
        )



-- Let Expression


letExpression : Parser (WithComments (Node Expression))
letExpression =
    ParserFast.keywordFollowedBy "let"
        (ParserFast.map3WithStartLocation
            (\start declarations commentsAfterIn expressionResult ->
                let
                    (Node expressionRange _) =
                        expressionResult |> Tuple.second
                in
                ( declarations
                    |> Tuple.first
                    |> Rope.prependTo commentsAfterIn
                    |> Rope.prependTo (expressionResult |> Tuple.first)
                , Node
                    { start = { row = start.row, column = start.column - 3 }
                    , end = expressionRange.end
                    }
                    (LetExpression
                        { declarations = declarations |> Tuple.second
                        , expression = expressionResult |> Tuple.second
                        }
                    )
                )
            )
            (ParserFast.withIndentSetToColumnMinus 3
                (ParserFast.map2
                    (\commentsAfterLet declarations ->
                        ( commentsAfterLet
                            |> Rope.prependTo (declarations |> Tuple.first)
                        , declarations |> Tuple.second
                        )
                    )
                    Layout.maybeLayout
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
            -- checks that the `in` token used as the end parser in letDeclarationsIn is indented correctly
            (Layout.positivelyIndentedPlusFollowedBy 2
                Layout.maybeLayout
            )
            expression
        )


letDeclarationsIn : Parser (WithComments (List (Node LetDeclaration)))
letDeclarationsIn =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map3
            (\headLetResult commentsAfter tailLetResult ->
                ( headLetResult
                    |> Tuple.first
                    |> Rope.prependTo commentsAfter
                    |> Rope.prependTo (tailLetResult |> Tuple.first)
                , (headLetResult |> Tuple.second) :: (tailLetResult |> Tuple.second)
                )
            )
            (ParserFast.oneOf2
                letFunction
                letDestructuringDeclaration
            )
            Layout.optimisticLayout
            (ParserWithComments.until Tokens.inToken blockElement)
        )


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map2
            (\( letDeclarationComments, letDeclarationSyntax ) commentsAfter ->
                ( letDeclarationComments |> Rope.prependTo commentsAfter
                , letDeclarationSyntax
                )
            )
            (ParserFast.oneOf2
                letFunction
                letDestructuringDeclaration
            )
            Layout.optimisticLayout
        )


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    ParserFast.map4
        (\( patternComments, pattern ) commentsAfterPattern commentsAfterEquals ( expressionComments, expressionSyntax ) ->
            let
                (Node { start } _) =
                    pattern

                (Node { end } _) =
                    expressionSyntax
            in
            ( patternComments
                |> Rope.prependTo commentsAfterPattern
                |> Rope.prependTo commentsAfterEquals
                |> Rope.prependTo expressionComments
            , Node { start = start, end = end }
                (LetDestructuring pattern expressionSyntax)
            )
        )
        Patterns.patternNotDirectlyComposing
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
            let
                allComments : Comments
                allComments =
                    (case maybeSignature of
                        Nothing ->
                            commentsAfterStartName

                        Just signature ->
                            commentsAfterStartName |> Rope.prependTo signature.comments
                    )
                        |> Rope.prependTo (arguments |> Tuple.first)
                        |> Rope.prependTo commentsAfterEqual
                        |> Rope.prependTo (expressionResult |> Tuple.first)
            in
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            expressionResult |> Tuple.second
                    in
                    ( allComments
                    , Node { start = startNameStart, end = expressionRange.end }
                        (LetFunction
                            { documentation = Nothing
                            , signature = Nothing
                            , declaration =
                                Node { start = startNameStart, end = expressionRange.end }
                                    { name = startNameNode
                                    , arguments = arguments |> Tuple.second
                                    , expression = expressionResult |> Tuple.second
                                    }
                            }
                        )
                    )

                Just signature ->
                    let
                        (Node implementationNameRange _) =
                            signature.implementationName

                        (Node expressionRange _) =
                            expressionResult |> Tuple.second
                    in
                    ( allComments
                    , Node { start = startNameStart, end = expressionRange.end }
                        (LetFunction
                            { documentation = Nothing
                            , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                            , declaration =
                                Node { start = implementationNameRange.start, end = expressionRange.end }
                                    { name = signature.implementationName
                                    , arguments = arguments |> Tuple.second
                                    , expression = expressionResult |> Tuple.second
                                    }
                            }
                        )
                    )
        )
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo (typeAnnotationResult |> Tuple.first)
                            |> Rope.prependTo (implementationName |> Tuple.first)
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName |> Tuple.second
                    , typeAnnotation = typeAnnotationResult |> Tuple.second
                    }
            )
            (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
            TypeAnnotation.typeAnnotation
            (Layout.layoutStrictFollowedBy
                Tokens.functionNameNode
            )
            Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Layout.maybeLayout
        expression
        |> ParserFast.validate
            (\result ->
                let
                    (Node _ letDeclaration) =
                        result |> Tuple.second
                in
                case letDeclaration of
                    LetDestructuring _ _ ->
                        True

                    LetFunction letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Node _ signature) ->
                                let
                                    (Node _ implementationName) =
                                        implementation.name

                                    (Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName ++ ""
            )
            "Expected to find the same name for declaration and signature"


parameterPatternsEqual : Parser (WithComments (List (Node Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (ParserFast.map2
            (\( patternComments, pattern ) commentsAfterPattern ->
                ( patternComments |> Rope.prependTo commentsAfterPattern
                , pattern
                )
            )
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
        )


numberExpression : Parser (WithComments (Node Expression))
numberExpression =
    ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
        (\range n ->
            ( Rope.empty
            , Node range (Floatable n)
            )
        )
        (\range n ->
            ( Rope.empty
            , Node range (Integer n)
            )
        )
        (\range n ->
            ( Rope.empty
            , Node range (Hex n)
            )
        )


ifBlockExpression : Parser (WithComments (Node Expression))
ifBlockExpression =
    ParserFast.keywordFollowedBy "if"
        (ParserFast.map8WithStartLocation
            (\start commentsAfterIf condition commentsBeforeThen commentsAfterThen ifTrue commentsBeforeElse commentsAfterElse ifFalse ->
                let
                    (Node ifFalseRange _) =
                        ifFalse |> Tuple.second
                in
                ( commentsAfterIf
                    |> Rope.prependTo (condition |> Tuple.first)
                    |> Rope.prependTo commentsBeforeThen
                    |> Rope.prependTo commentsAfterThen
                    |> Rope.prependTo (ifTrue |> Tuple.first)
                    |> Rope.prependTo commentsBeforeElse
                    |> Rope.prependTo commentsAfterElse
                    |> Rope.prependTo (ifFalse |> Tuple.first)
                , Node
                    { start = { row = start.row, column = start.column - 2 }
                    , end = ifFalseRange.end
                    }
                    (IfBlock
                        (condition |> Tuple.second)
                        (ifTrue |> Tuple.second)
                        (ifFalse |> Tuple.second)
                    )
                )
            )
            Layout.maybeLayout
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "then" Layout.maybeLayout)
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "else" Layout.maybeLayout)
            expression
        )


negationOperation : Parser (WithComments (Node Expression))
negationOperation =
    ParserFast.symbolBacktrackableFollowedBy "-"
        (ParserFast.offsetSourceAndThen
            (\offset source ->
                case String.slice (offset - 2) (offset - 1) source of
                    " " ->
                        negationAfterMinus

                    -- not "\n" or "\r" since expressions are always indented
                    "(" ->
                        negationAfterMinus

                    ")" ->
                        negationAfterMinus

                    -- from the end of a multiline comment
                    "}" ->
                        negationAfterMinus

                    -- TODO only for tests
                    "" ->
                        negationAfterMinus

                    _ ->
                        negationWhitespaceProblem
            )
        )


negationWhitespaceProblem : Parser a
negationWhitespaceProblem =
    ParserFast.problem "if a negation sign is not preceded by whitespace, it's considered subtraction"


negationAfterMinus : Parser (WithComments (Node Expression))
negationAfterMinus =
    ParserFast.map
        (\subExpressionResult ->
            let
                (Node subExpressionRange _) =
                    subExpressionResult |> Tuple.second
            in
            ( subExpressionResult |> Tuple.first
            , Node
                { start =
                    { row = subExpressionRange.start.row
                    , column = subExpressionRange.start.column - 1
                    }
                , end = subExpressionRange.end
                }
                (Negation (subExpressionResult |> Tuple.second))
            )
        )
        subExpression


qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            ( Rope.empty
            , Node range
                (case after of
                    Nothing ->
                        FunctionOrValue [] firstName

                    Just ( qualificationAfter, unqualified ) ->
                        FunctionOrValue (firstName :: qualificationAfter) unqualified
                )
            )
        )
        Tokens.typeName
        maybeDotReferenceExpressionTuple
        |> followedByMultiRecordAccess


maybeDotReferenceExpressionTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2Map
                Just
                (ParserFast.map2
                    (\firstName after ->
                        case after of
                            Nothing ->
                                ( [], firstName )

                            Just ( qualificationAfter, unqualified ) ->
                                ( firstName :: qualificationAfter, unqualified )
                    )
                    Tokens.typeName
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                (\name -> Just ( [], name ))
                Tokens.functionName
            )
        )
        Nothing


unqualifiedFunctionReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
unqualifiedFunctionReferenceExpressionFollowedByRecordAccess =
    Tokens.functionNameMapWithRange
        (\range unqualified ->
            ( Rope.empty
            , Node range (FunctionOrValue [] unqualified)
            )
        )
        |> followedByMultiRecordAccess


recordAccessFunctionExpression : Parser (WithComments (Node Expression))
recordAccessFunctionExpression =
    ParserFast.symbolFollowedBy "."
        (Tokens.functionNameMapWithRange
            (\range field ->
                ( Rope.empty
                , Node (range |> rangeMoveStartLeftByOneColumn)
                    (RecordAccessFunction ("." ++ field))
                )
            )
        )


rangeMoveStartLeftByOneColumn : Range -> Range
rangeMoveStartLeftByOneColumn range =
    { start = { row = range.start.row, column = range.start.column - 1 }
    , end = range.end
    }


tupledExpressionIfNecessaryFollowedByRecordAccess : Parser (WithComments (Node Expression))
tupledExpressionIfNecessaryFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf3
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    ( Rope.empty
                    , Node { start = { row = end.row, column = end.column - 2 }, end = end }
                        UnitExpr
                    )
                )
            )
            allowedPrefixOperatorFollowedByClosingParensOneOf
            tupledExpressionInnerAfterOpeningParens
        )


allowedPrefixOperatorFollowedByClosingParensOneOf : Parser (WithComments (Node Expression))
allowedPrefixOperatorFollowedByClosingParensOneOf =
    ParserFast.whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
        (\operatorRange operator ->
            ( Rope.empty
            , Node
                { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                }
                (PrefixOperator operator)
            )
        )
        Tokens.isOperatorSymbolChar
        Tokens.isAllowedOperatorToken
        ")"


tupledExpressionInnerAfterOpeningParens : Parser (WithComments (Node Expression))
tupledExpressionInnerAfterOpeningParens =
    ParserFast.map4WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart ( firstPartComments, firstPart ) commentsAfterFirstPart ( tailPartsComments, tailParts ) ->
            ( commentsBeforeFirstPart
                |> Rope.prependTo firstPartComments
                |> Rope.prependTo commentsAfterFirstPart
                |> Rope.prependTo tailPartsComments
            , case tailParts of
                TupledParenthesized () () ->
                    Node
                        { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                        , end = rangeAfterOpeningParens.end
                        }
                        (ParenthesizedExpression firstPart)

                TupledTwoOrThree secondPart maybeThirdPart ->
                    Node
                        { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                        , end = rangeAfterOpeningParens.end
                        }
                        (case maybeThirdPart of
                            Nothing ->
                                TupledExpression [ firstPart, secondPart ]

                            Just thirdPart ->
                                TupledExpression [ firstPart, secondPart, thirdPart ]
                        )
            )
        )
        Layout.maybeLayout
        expression
        Layout.maybeLayout
        (ParserFast.oneOf2
            (ParserFast.symbol ")"
                ( Rope.empty, TupledParenthesized () () )
            )
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map4
                    (\commentsBefore partResult commentsAfter maybeThirdPart ->
                        ( commentsBefore
                            |> Rope.prependTo (partResult |> Tuple.first)
                            |> Rope.prependTo commentsAfter
                            |> Rope.prependTo (maybeThirdPart |> Tuple.first)
                        , TupledTwoOrThree (partResult |> Tuple.second) (maybeThirdPart |> Tuple.second)
                        )
                    )
                    Layout.maybeLayout
                    expression
                    Layout.maybeLayout
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" ( Rope.empty, Nothing ))
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map3
                                (\commentsBefore partResult commentsAfter ->
                                    ( commentsBefore
                                        |> Rope.prependTo (partResult |> Tuple.first)
                                        |> Rope.prependTo commentsAfter
                                    , Just (partResult |> Tuple.second)
                                    )
                                )
                                Layout.maybeLayout
                                expression
                                Layout.maybeLayout
                                |> ParserFast.followedBySymbol ")"
                            )
                        )
                    )
                )
            )
        )
        |> followedByMultiRecordAccess


type Tupled
    = TupledParenthesized () ()
    | TupledTwoOrThree (Node Expression) (Maybe (Node Expression))



---


extendedSubExpressionOptimisticLayout :
    (InfixOperatorInfo -> Result String intermediate)
    -> (intermediate -> Parser (WithComments ExtensionRight))
    -> Parser (WithComments (Node Expression))
extendedSubExpressionOptimisticLayout toResult afterCommitting =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (Layout.positivelyIndentedFollowedBy
            (infixOperatorAndThen toResult afterCommitting)
        )
        subExpressionMaybeAppliedOptimisticLayout
        (\( extensionRightComments, extensionRight ) ( leftNodeComments, leftNode ) ->
            ( leftNodeComments
                |> Rope.prependTo extensionRightComments
            , leftNode |> applyExtensionRight extensionRight
            )
        )
        Basics.identity


infixOperatorAndThen : (InfixOperatorInfo -> Result String intermediate) -> (intermediate -> Parser res) -> Parser res
infixOperatorAndThen toResult afterCommitting =
    ParserFast.whileWithoutLinebreakAnd2PartUtf16ToResultAndThen
        Tokens.isOperatorSymbolChar
        (\operator ->
            case operator of
                "|>" ->
                    toResult precedence1ApR

                "++" ->
                    toResult precedence5append

                "<|" ->
                    toResult precedence1ApL

                ">>" ->
                    toResult precedence9ComposeR

                "==" ->
                    toResult precedence4Eq

                "*" ->
                    toResult precedence7Mul

                "::" ->
                    toResult precedence5Cons

                "+" ->
                    toResult precedence6Add

                "-" ->
                    toResult precedence6Sub

                "|." ->
                    toResult precedence6Ignore

                "&&" ->
                    toResult precedence3And

                "|=" ->
                    toResult precedence5Keep

                "<<" ->
                    toResult precedence9ComposeL

                "/=" ->
                    toResult precedence4Neq

                "//" ->
                    toResult precedence7Idiv

                "/" ->
                    toResult precedence7Fdiv

                "</>" ->
                    toResult precedence7Slash

                "||" ->
                    toResult precedence2Or

                "<=" ->
                    toResult precedence4Le

                ">=" ->
                    toResult precedence4Ge

                ">" ->
                    toResult precedence4Gt

                "<?>" ->
                    toResult precedence8QuestionMark

                "<" ->
                    toResult precedence4Lt

                "^" ->
                    toResult precedence8Pow

                _ ->
                    errUnknownInfixOperator
        )
        afterCommitting


subExpressionMaybeAppliedOptimisticLayout : Parser (WithComments (Node Expression))
subExpressionMaybeAppliedOptimisticLayout =
    ParserFast.map3
        (\( leftExpressionComments, leftNode ) commentsBeforeExtension ( maybeArgsComments, maybeArgsReverse ) ->
            ( leftExpressionComments
                |> Rope.prependTo commentsBeforeExtension
                |> Rope.prependTo maybeArgsComments
            , case maybeArgsReverse of
                [] ->
                    leftNode

                ((Node lastArgRange _) :: _) as argsReverse ->
                    let
                        (Node leftRange _) =
                            leftNode
                    in
                    Node { start = leftRange.start, end = lastArgRange.end }
                        (Expression.Application
                            (leftNode :: List.reverse argsReverse)
                        )
            )
        )
        subExpression
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\( argComments, arg ) commentsAfter ->
                        ( argComments |> Rope.prependTo commentsAfter
                        , arg
                        )
                    )
                    subExpression
                    Layout.optimisticLayout
                )
            )
        )


applyExtensionRight : ExtensionRight -> Node Expression -> Node Expression
applyExtensionRight (ExtendRightByOperation operation) ((Node leftRange _) as leftNode) =
    let
        ((Node rightExpressionRange _) as rightExpressionNode) =
            operation.expression
    in
    Node { start = leftRange.start, end = rightExpressionRange.end }
        (OperatorApplication operation.symbol
            operation.direction
            leftNode
            rightExpressionNode
        )


type alias InfixOperatorInfo =
    { leftPrecedence : Int
    , symbol : String
    , extensionRight : Parser (WithComments ExtensionRight)
    }


errUnknownInfixOperator : Result String a
errUnknownInfixOperator =
    Err "unknown infix operator"


infixLeft : Int -> String -> InfixOperatorInfo
infixLeft leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRight =
        ParserFast.map2
            (\commentsBeforeFirst ( firstComments, first ) ->
                ( commentsBeforeFirst
                    |> Rope.prependTo firstComments
                , ExtendRightByOperation
                    { symbol = symbol
                    , direction = Infix.Left
                    , expression = first
                    }
                )
            )
            Layout.maybeLayout
            (extendedSubExpressionOptimisticLayout
                (\info ->
                    if info.leftPrecedence > leftPrecedence then
                        Ok info

                    else
                        temporaryErrPrecedenceTooHigh
                )
                .extensionRight
            )
    }


infixNonAssociative : Int -> String -> InfixOperatorInfo
infixNonAssociative leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRight =
        ParserFast.map2
            (\commentsBefore ( rightComments, right ) ->
                ( commentsBefore |> Rope.prependTo rightComments
                , ExtendRightByOperation
                    { symbol = symbol
                    , direction = Infix.Non
                    , expression = right
                    }
                )
            )
            Layout.maybeLayout
            (extendedSubExpressionOptimisticLayout
                (\info ->
                    if info.leftPrecedence >= leftPrecedence then
                        Ok info

                    else
                        temporaryErrPrecedenceTooHigh
                )
                (\info ->
                    if info.leftPrecedence == leftPrecedence then
                        problemCannotMixNonAssociativeInfixOperators

                    else
                        -- info.leftPrecedence > leftPrecedence
                        info.extensionRight
                )
            )
    }


problemCannotMixNonAssociativeInfixOperators : Parser a
problemCannotMixNonAssociativeInfixOperators =
    ParserFast.problem "cannot mix non-associative infix operators without parenthesis"


infixRight : Int -> String -> InfixOperatorInfo
infixRight leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRight =
        ParserFast.map2
            (\commentsBeforeFirst ( firstComments, first ) ->
                ( commentsBeforeFirst
                    |> Rope.prependTo firstComments
                , ExtendRightByOperation
                    { symbol = symbol
                    , direction = Infix.Right
                    , expression = first
                    }
                )
            )
            Layout.maybeLayout
            (extendedSubExpressionOptimisticLayout
                (\info ->
                    if info.leftPrecedence >= leftPrecedence then
                        Ok info

                    else
                        temporaryErrPrecedenceTooHigh
                )
                .extensionRight
            )
    }


temporaryErrPrecedenceTooHigh : Result String a
temporaryErrPrecedenceTooHigh =
    Err "infix operator precedence too high"


type ExtensionRight
    = ExtendRightByOperation
        { symbol : String
        , direction : Infix.InfixDirection
        , expression : Node Expression
        }
