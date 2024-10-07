module Elm.Parser.Expression exposing (expressionFollowedByOptimisticLayout)

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
import ParserWithComments exposing (WithComments)
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
                    lambdaExpressionFollowedByOptimisticLayout

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
        caseExpressionFollowedByOptimisticLayout
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


letOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
letOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        letExpressionFollowedByOptimisticLayout
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


ifOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
ifOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        ifBlockExpressionFollowedByOptimisticLayout
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
        (\((Node fieldRange _) as fieldNode) leftResult ->
            let
                ((Node leftRange _) as leftNode) =
                    leftResult.syntax
            in
            { comments = leftResult.comments
            , syntax =
                Node { start = leftRange.start, end = fieldRange.end }
                    (Expression.RecordAccess leftNode fieldNode)
            }
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


expressionFollowedByOptimisticLayout : Parser (WithComments (Node Expression))
expressionFollowedByOptimisticLayout =
    extendedSubExpressionOptimisticLayout
        { afterCommitting = .extensionRightParser
        , validateRightPrecedence = Ok
        }


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithRange
            (\range s ->
                { comments = Rope.empty
                , syntax =
                    Node
                        -- TODO for v8: don't include extra end width (from bug in elm/parser) in range
                        { start = { row = range.start.row, column = range.start.column - 6 }
                        , end = { row = range.end.row, column = range.end.column + 2 }
                        }
                        (GLSLExpression s)
                }
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
            (\range commentsBefore elements ->
                { comments = commentsBefore |> Rope.prependTo elements.comments
                , syntax =
                    Node
                        { start = { row = range.start.row, column = range.start.column - 1 }
                        , end = range.end
                        }
                        elements.syntax
                }
            )
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbol "]" { comments = Rope.empty, syntax = ListExpr [] })
                (ParserFast.map2
                    (\head tail ->
                        { comments =
                            head.comments
                                |> Rope.prependTo tail.comments
                        , syntax = ListExpr (head.syntax :: tail.syntax)
                        }
                    )
                    expressionFollowedByOptimisticLayout
                    (Layout.positivelyIndentedFollowedBy
                        (ParserWithComments.many
                            (ParserFast.symbolFollowedBy ","
                                (ParserFast.map2
                                    (\commentsBefore expressionResult ->
                                        { comments = commentsBefore |> Rope.prependTo expressionResult.comments
                                        , syntax = expressionResult.syntax
                                        }
                                    )
                                    Layout.maybeLayout
                                    expressionFollowedByOptimisticLayout
                                    |> Layout.endsPositivelyIndented
                                )
                            )
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
            (\range commentsBefore afterCurly ->
                { comments =
                    commentsBefore
                        |> Rope.prependTo afterCurly.comments
                , syntax = Node (rangeMoveStartLeftByOneColumn range) afterCurly.syntax
                }
            )
            Layout.maybeLayout
            recordContentsCurlyEnd
            |> followedByMultiRecordAccess
        )


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    ParserFast.oneOf2
        (ParserFast.map5
            (\nameNode commentsAfterFunctionName afterNameBeforeFields tailFields commentsBeforeClosingCurly ->
                { comments =
                    commentsAfterFunctionName
                        |> Rope.prependTo afterNameBeforeFields.comments
                        |> Rope.prependTo tailFields.comments
                        |> Rope.prependTo commentsBeforeClosingCurly
                , syntax =
                    case afterNameBeforeFields.syntax of
                        RecordUpdateFirstSetter firstField ->
                            RecordUpdateExpression nameNode (firstField :: tailFields.syntax)

                        FieldsFirstValue firstFieldValue ->
                            RecordExpr (Node.combine Tuple.pair nameNode firstFieldValue :: tailFields.syntax)
                }
            )
            Tokens.functionNameNode
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map2
                        (\commentsBefore setterResult ->
                            { comments = commentsBefore |> Rope.prependTo setterResult.comments
                            , syntax = RecordUpdateFirstSetter setterResult.syntax
                            }
                        )
                        Layout.maybeLayout
                        recordSetterNodeWithLayout
                    )
                )
                (ParserFast.symbolFollowedBy "="
                    (ParserFast.map2
                        (\commentsBefore expressionResult ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo expressionResult.comments
                            , syntax = FieldsFirstValue expressionResult.syntax
                            }
                        )
                        Layout.maybeLayout
                        expressionFollowedByOptimisticLayout
                        |> Layout.endsPositivelyIndented
                    )
                )
            )
            recordFields
            (Layout.maybeLayout |> ParserFast.followedBySymbol "}")
        )
        (ParserFast.symbol "}" { comments = Rope.empty, syntax = RecordExpr [] })


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        (ParserFast.symbolFollowedBy ","
            (ParserFast.map2
                (\commentsBefore setterResult ->
                    { comments = commentsBefore |> Rope.prependTo setterResult.comments
                    , syntax = setterResult.syntax
                    }
                )
                Layout.maybeLayout
                recordSetterNodeWithLayout
            )
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    ParserFast.map4WithRange
        (\range name commentsAfterFunctionName commentsAfterEquals expressionResult ->
            { comments =
                commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
            , syntax = Node range ( name, expressionResult.syntax )
            }
        )
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove
        |> Layout.endsPositivelyIndented


literalExpression : Parser (WithComments (Node Expression))
literalExpression =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Node range (Literal string)
            }
        )


charLiteralExpression : Parser (WithComments (Node Expression))
charLiteralExpression =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            { comments = Rope.empty
            , syntax = Node range (CharLiteral char)
            }
        )



-- lambda


lambdaExpressionFollowedByOptimisticLayout : Parser (WithComments (Node Expression))
lambdaExpressionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\start commentsAfterBackslash firstArg commentsAfterFirstArg secondUpArgs commentsAfterArrow expressionResult ->
            let
                (Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                commentsAfterBackslash
                    |> Rope.prependTo firstArg.comments
                    |> Rope.prependTo commentsAfterFirstArg
                    |> Rope.prependTo secondUpArgs.comments
                    |> Rope.prependTo commentsAfterArrow
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Node
                    { start = start
                    , end = expressionRange.end
                    }
                    (LambdaExpression
                        { args = firstArg.syntax :: secondUpArgs.syntax
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.symbolFollowedBy "\\" Layout.maybeLayout)
        Patterns.patternNotDirectlyComposing
        Layout.maybeLayout
        (ParserWithComments.until
            (ParserFast.symbol "->" ())
            (ParserFast.map2
                (\patternResult commentsAfter ->
                    { comments =
                        patternResult.comments
                            |> Rope.prependTo commentsAfter
                    , syntax = patternResult.syntax
                    }
                )
                Patterns.patternNotDirectlyComposing
                Layout.maybeLayout
            )
        )
        Layout.maybeLayout
        expressionFollowedByOptimisticLayout



-- Case Expression


caseExpressionFollowedByOptimisticLayout : Parser (WithComments (Node Expression))
caseExpressionFollowedByOptimisticLayout =
    ParserFast.map4WithStartLocation
        (\start commentsAfterCase casedExpressionResult commentsAfterOf casesResult ->
            let
                ( firstCase, lastToSecondCase ) =
                    casesResult.syntax
            in
            { comments =
                commentsAfterCase
                    |> Rope.prependTo casedExpressionResult.comments
                    |> Rope.prependTo commentsAfterOf
                    |> Rope.prependTo casesResult.comments
            , syntax =
                Node
                    { start = start
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
                        { expression = casedExpressionResult.syntax
                        , cases = firstCase :: List.reverse lastToSecondCase
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "case" Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        (Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "of" Layout.maybeLayout)
        )
        (ParserFast.withIndentSetToColumn caseStatementsFollowedByOptimisticLayout)


caseStatementsFollowedByOptimisticLayout : Parser (WithComments ( Case, List Case ))
caseStatementsFollowedByOptimisticLayout =
    ParserFast.map5
        (\firstCasePatternResult commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
            { comments =
                firstCasePatternResult.comments
                    |> Rope.prependTo commentsAfterFirstCasePattern
                    |> Rope.prependTo commentsAfterFirstCaseArrowRight
                    |> Rope.prependTo firstCaseExpressionResult.comments
                    |> Rope.prependTo lastToSecondCase.comments
            , syntax =
                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                , lastToSecondCase.syntax
                )
            }
        )
        Patterns.pattern
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "->" Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        (ParserWithComments.manyWithoutReverse caseStatementFollowedByOptimisticLayout)


caseStatementFollowedByOptimisticLayout : Parser (WithComments Case)
caseStatementFollowedByOptimisticLayout =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map4
            (\pattern commentsBeforeArrowRight commentsAfterArrowRight expr ->
                { comments =
                    pattern.comments
                        |> Rope.prependTo commentsBeforeArrowRight
                        |> Rope.prependTo commentsAfterArrowRight
                        |> Rope.prependTo expr.comments
                , syntax = ( pattern.syntax, expr.syntax )
                }
            )
            Patterns.pattern
            Layout.maybeLayout
            (ParserFast.symbolFollowedBy "->" Layout.maybeLayout)
            expressionFollowedByOptimisticLayout
        )



-- Let Expression


letExpressionFollowedByOptimisticLayout : Parser (WithComments (Node Expression))
letExpressionFollowedByOptimisticLayout =
    ParserFast.map3WithStartLocation
        (\start declarations commentsAfterIn expressionResult ->
            let
                (Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                declarations.comments
                    |> Rope.prependTo commentsAfterIn
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Node
                    { start = start
                    , end = expressionRange.end
                    }
                    (LetExpression
                        { declarations = declarations.declarations
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.withIndentSetToColumn
            (ParserFast.keywordFollowedBy "let"
                (ParserFast.map2
                    (\commentsAfterLet declarations ->
                        { comments =
                            commentsAfterLet
                                |> Rope.prependTo declarations.comments
                        , declarations = declarations.syntax
                        }
                    )
                    Layout.maybeLayout
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
        )
        -- checks that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        (Layout.positivelyIndentedPlusFollowedBy 2
            Layout.maybeLayout
        )
        expressionFollowedByOptimisticLayout


letDeclarationsIn : Parser (WithComments (List (Node LetDeclaration)))
letDeclarationsIn =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map3
            (\headLetResult commentsAfter tailLetResult ->
                { comments =
                    headLetResult.comments
                        |> Rope.prependTo commentsAfter
                        |> Rope.prependTo tailLetResult.comments
                , syntax = headLetResult.syntax :: tailLetResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunctionFollowedByOptimisticLayout
                letDestructuringDeclarationFollowedByOptimisticLayout
            )
            Layout.optimisticLayout
            (ParserWithComments.until Tokens.inToken letBlockElementFollowedByOptimisticLayout)
        )


letBlockElementFollowedByOptimisticLayout : Parser (WithComments (Node LetDeclaration))
letBlockElementFollowedByOptimisticLayout =
    Layout.onTopIndentationFollowedBy
        (ParserFast.oneOf2
            letFunctionFollowedByOptimisticLayout
            letDestructuringDeclarationFollowedByOptimisticLayout
        )


letDestructuringDeclarationFollowedByOptimisticLayout : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclarationFollowedByOptimisticLayout =
    ParserFast.map4
        (\pattern commentsAfterPattern commentsAfterEquals expressionResult ->
            let
                (Node { start } _) =
                    pattern.syntax

                (Node { end } _) =
                    expressionResult.syntax
            in
            { comments =
                pattern.comments
                    |> Rope.prependTo commentsAfterPattern
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Node { start = start, end = end }
                    (LetDestructuring pattern.syntax expressionResult.syntax)
            }
        )
        Patterns.patternNotDirectlyComposing
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        expressionFollowedByOptimisticLayout


letFunctionFollowedByOptimisticLayout : Parser (WithComments (Node LetDeclaration))
letFunctionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            expressionResult.syntax
                    in
                    { comments =
                        commentsAfterStartName
                            |> Rope.prependTo arguments.comments
                            |> Rope.prependTo commentsAfterEqual
                            |> Rope.prependTo expressionResult.comments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (LetFunction
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = startNameStart, end = expressionRange.end }
                                        { name = startNameNode
                                        , arguments = arguments.syntax
                                        , expression = expressionResult.syntax
                                        }
                                }
                            )
                    }

                Just signature ->
                    let
                        (Node implementationNameRange _) =
                            signature.implementationName

                        (Node expressionRange _) =
                            expressionResult.syntax
                    in
                    { comments =
                        (commentsAfterStartName |> Rope.prependTo signature.comments)
                            |> Rope.prependTo arguments.comments
                            |> Rope.prependTo commentsAfterEqual
                            |> Rope.prependTo expressionResult.comments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (LetFunction
                                { documentation = Nothing
                                , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                , declaration =
                                    Node { start = implementationNameRange.start, end = expressionRange.end }
                                        { name = signature.implementationName
                                        , arguments = arguments.syntax
                                        , expression = expressionResult.syntax
                                        }
                                }
                            )
                    }
        )
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
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
        expressionFollowedByOptimisticLayout
        |> ParserFast.validate
            (\result ->
                let
                    (Node _ letDeclaration) =
                        result.syntax
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
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
        )


numberExpression : Parser (WithComments (Node Expression))
numberExpression =
    ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
        (\range n ->
            { comments = Rope.empty
            , syntax = Node range (Floatable n)
            }
        )
        (\range n ->
            { comments = Rope.empty
            , syntax = Node range (Integer n)
            }
        )
        (\range n ->
            { comments = Rope.empty
            , syntax = Node range (Hex n)
            }
        )


ifBlockExpressionFollowedByOptimisticLayout : Parser (WithComments (Node Expression))
ifBlockExpressionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\start commentsAfterIf condition commentsAfterThen ifTrue commentsAfterElse ifFalse ->
            let
                (Node ifFalseRange _) =
                    ifFalse.syntax
            in
            { comments =
                commentsAfterIf
                    |> Rope.prependTo condition.comments
                    |> Rope.prependTo commentsAfterThen
                    |> Rope.prependTo ifTrue.comments
                    |> Rope.prependTo commentsAfterElse
                    |> Rope.prependTo ifFalse.comments
            , syntax =
                Node
                    { start = start
                    , end = ifFalseRange.end
                    }
                    (IfBlock
                        condition.syntax
                        ifTrue.syntax
                        ifFalse.syntax
                    )
            }
        )
        (ParserFast.keywordFollowedBy "if" Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        (Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "then" Layout.maybeLayout)
        )
        expressionFollowedByOptimisticLayout
        (Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "else" Layout.maybeLayout)
        )
        expressionFollowedByOptimisticLayout


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
                    subExpressionResult.syntax
            in
            { comments = subExpressionResult.comments
            , syntax =
                Node
                    { start =
                        { row = subExpressionRange.start.row
                        , column = subExpressionRange.start.column - 1
                        }
                    , end = subExpressionRange.end
                    }
                    (Negation subExpressionResult.syntax)
            }
        )
        subExpression


qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = Rope.empty
            , syntax =
                Node range
                    (case after of
                        Nothing ->
                            FunctionOrValue [] firstName

                        Just ( qualificationAfter, unqualified ) ->
                            FunctionOrValue (firstName :: qualificationAfter) unqualified
                    )
            }
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
            { comments = Rope.empty
            , syntax =
                Node range (FunctionOrValue [] unqualified)
            }
        )
        |> followedByMultiRecordAccess


recordAccessFunctionExpression : Parser (WithComments (Node Expression))
recordAccessFunctionExpression =
    ParserFast.symbolFollowedBy "."
        (Tokens.functionNameMapWithRange
            (\range field ->
                { comments = Rope.empty
                , syntax =
                    Node (range |> rangeMoveStartLeftByOneColumn)
                        (RecordAccessFunction ("." ++ field))
                }
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
                    { comments = Rope.empty
                    , syntax =
                        Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            UnitExpr
                    }
                )
            )
            allowedPrefixOperatorFollowedByClosingParensOneOf
            tupledExpressionInnerAfterOpeningParens
        )


allowedPrefixOperatorFollowedByClosingParensOneOf : Parser (WithComments (Node Expression))
allowedPrefixOperatorFollowedByClosingParensOneOf =
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
        (\operatorRange operator ->
            { comments = Rope.empty
            , syntax =
                Node
                    { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                    , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                    }
                    (PrefixOperator operator)
            }
        )
        Tokens.isOperatorSymbolCharAsString
        Tokens.isAllowedOperatorToken
        ")"


tupledExpressionInnerAfterOpeningParens : Parser (WithComments (Node Expression))
tupledExpressionInnerAfterOpeningParens =
    ParserFast.map3WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart tailParts ->
            { comments =
                commentsBeforeFirstPart
                    |> Rope.prependTo firstPart.comments
                    |> Rope.prependTo tailParts.comments
            , syntax =
                case tailParts.syntax of
                    TupledParenthesized () () ->
                        Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (ParenthesizedExpression firstPart.syntax)

                    TupledTwoOrThree secondPart maybeThirdPart ->
                        Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case maybeThirdPart of
                                Nothing ->
                                    TupledExpression [ firstPart.syntax, secondPart ]

                                Just thirdPart ->
                                    TupledExpression [ firstPart.syntax, secondPart, thirdPart ]
                            )
            }
        )
        Layout.maybeLayout
        expressionFollowedByOptimisticLayout
        (Layout.positivelyIndentedFollowedBy
            (ParserFast.oneOf2
                (ParserFast.symbol ")"
                    { comments = Rope.empty, syntax = TupledParenthesized () () }
                )
                (ParserFast.symbolFollowedBy ","
                    (ParserFast.map3
                        (\commentsBefore partResult maybeThirdPart ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo partResult.comments
                                    |> Rope.prependTo maybeThirdPart.comments
                            , syntax = TupledTwoOrThree partResult.syntax maybeThirdPart.syntax
                            }
                        )
                        Layout.maybeLayout
                        expressionFollowedByOptimisticLayout
                        (Layout.positivelyIndentedFollowedBy
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore partResult ->
                                            { comments =
                                                commentsBefore
                                                    |> Rope.prependTo partResult.comments
                                            , syntax = Just partResult.syntax
                                            }
                                        )
                                        Layout.maybeLayout
                                        expressionFollowedByOptimisticLayout
                                        |> Layout.endsPositivelyIndented
                                        |> ParserFast.followedBySymbol ")"
                                    )
                                )
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
    { info
        | afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
        , validateRightPrecedence : InfixOperatorInfo -> Result String InfixOperatorInfo
    }
    -> Parser (WithComments (Node Expression))
extendedSubExpressionOptimisticLayout info =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (Layout.positivelyIndentedFollowedBy
            (infixOperatorAndThen info)
        )
        subExpressionMaybeAppliedFollowedByOptimisticLayout
        (\extensionRightResult leftNodeWithComments ->
            { comments =
                leftNodeWithComments.comments
                    |> Rope.prependTo extensionRightResult.comments
            , syntax =
                leftNodeWithComments.syntax
                    |> applyExtensionRight extensionRightResult.syntax
            }
        )
        Basics.identity


problemCannotMixNonAssociativeInfixOperators : Parser a
problemCannotMixNonAssociativeInfixOperators =
    ParserFast.problem "cannot mix non-associative infix operators without parenthesis"


temporaryErrPrecedenceTooHigh : Result String a
temporaryErrPrecedenceTooHigh =
    Err "infix operator precedence too high"


extensionRightParser :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , direction : Infix.InfixDirection
    , symbol : String
    , validateRightPrecedence : InfixOperatorInfo -> Result String InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
extensionRightParser extensionRightInfo =
    ParserFast.map2
        (\commentsBefore right ->
            { comments = commentsBefore |> Rope.prependTo right.comments
            , syntax =
                ExtendRightByOperation
                    { symbol = extensionRightInfo.symbol
                    , direction = extensionRightInfo.direction
                    , expression = right.syntax
                    }
            }
        )
        Layout.maybeLayout
        (ParserFast.lazy
            (\() -> extendedSubExpressionOptimisticLayout extensionRightInfo)
        )


infixOperatorAndThen :
    { info
        | afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
        , validateRightPrecedence : InfixOperatorInfo -> Result String InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
infixOperatorAndThen extensionRightConstraints =
    let
        toResult : InfixOperatorInfo -> Result String InfixOperatorInfo
        toResult =
            extensionRightConstraints.validateRightPrecedence

        apRResult : Result String InfixOperatorInfo
        apRResult =
            toResult precedence1ApR

        appendResult : Result String InfixOperatorInfo
        appendResult =
            toResult precedence5append

        apLResult : Result String InfixOperatorInfo
        apLResult =
            toResult precedence1ApL

        composeRResult : Result String InfixOperatorInfo
        composeRResult =
            toResult precedence9ComposeR

        eqResult : Result String InfixOperatorInfo
        eqResult =
            toResult precedence4Eq

        mulResult : Result String InfixOperatorInfo
        mulResult =
            toResult precedence7Mul

        consResult : Result String InfixOperatorInfo
        consResult =
            toResult precedence5Cons

        addResult : Result String InfixOperatorInfo
        addResult =
            toResult precedence6Add

        subResult : Result String InfixOperatorInfo
        subResult =
            toResult precedence6Sub

        ignoreResult : Result String InfixOperatorInfo
        ignoreResult =
            toResult precedence6Ignore

        andResult : Result String InfixOperatorInfo
        andResult =
            toResult precedence3And

        keepResult : Result String InfixOperatorInfo
        keepResult =
            toResult precedence5Keep

        composeLResult : Result String InfixOperatorInfo
        composeLResult =
            toResult precedence9ComposeL

        neqResult : Result String InfixOperatorInfo
        neqResult =
            toResult precedence4Neq

        idivResult : Result String InfixOperatorInfo
        idivResult =
            toResult precedence7Idiv

        fdivResult : Result String InfixOperatorInfo
        fdivResult =
            toResult precedence7Fdiv

        slashResult : Result String InfixOperatorInfo
        slashResult =
            toResult precedence7Slash

        orResult : Result String InfixOperatorInfo
        orResult =
            toResult precedence2Or

        leResult : Result String InfixOperatorInfo
        leResult =
            toResult precedence4Le

        geResult : Result String InfixOperatorInfo
        geResult =
            toResult precedence4Ge

        gtResult : Result String InfixOperatorInfo
        gtResult =
            toResult precedence4Gt

        questionMarkResult : Result String InfixOperatorInfo
        questionMarkResult =
            toResult precedence8QuestionMark

        ltResult : Result String InfixOperatorInfo
        ltResult =
            toResult precedence4Lt

        powResult : Result String InfixOperatorInfo
        powResult =
            toResult precedence8Pow
    in
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ToResultAndThen
        Tokens.isOperatorSymbolCharAsString
        (\operator ->
            case operator of
                "|>" ->
                    apRResult

                "++" ->
                    appendResult

                "<|" ->
                    apLResult

                ">>" ->
                    composeRResult

                "==" ->
                    eqResult

                "*" ->
                    mulResult

                "::" ->
                    consResult

                "+" ->
                    addResult

                "-" ->
                    subResult

                "|." ->
                    ignoreResult

                "&&" ->
                    andResult

                "|=" ->
                    keepResult

                "<<" ->
                    composeLResult

                "/=" ->
                    neqResult

                "//" ->
                    idivResult

                "/" ->
                    fdivResult

                "</>" ->
                    slashResult

                "||" ->
                    orResult

                "<=" ->
                    leResult

                ">=" ->
                    geResult

                ">" ->
                    gtResult

                "<?>" ->
                    questionMarkResult

                "<" ->
                    ltResult

                "^" ->
                    powResult

                _ ->
                    errUnknownInfixOperator
        )
        extensionRightConstraints.afterCommitting


subExpressionMaybeAppliedFollowedByOptimisticLayout : Parser (WithComments (Node Expression))
subExpressionMaybeAppliedFollowedByOptimisticLayout =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    literalExpressionOptimisticLayout

                "(" ->
                    tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied

                "[" ->
                    listOrGlslExpressionOptimisticLayout

                "{" ->
                    recordExpressionFollowedByRecordAccessMaybeApplied

                "c" ->
                    caseOrUnqualifiedReferenceExpressionMaybeApplied

                "\\" ->
                    lambdaExpressionFollowedByOptimisticLayout

                "l" ->
                    letOrUnqualifiedReferenceExpressionMaybeApplied

                "i" ->
                    ifOrUnqualifiedReferenceExpressionMaybeApplied

                "." ->
                    recordAccessFunctionExpressionMaybeApplied

                "-" ->
                    negationOperationOptimisticLayout

                "'" ->
                    charLiteralExpressionOptimisticLayout

                _ ->
                    referenceOrNumberExpressionMaybeApplied
        )


negationOperationOptimisticLayout : Parser (WithComments (Node Expression))
negationOperationOptimisticLayout =
    negationOperation |> followedByOptimisticLayout


charLiteralExpressionOptimisticLayout : Parser (WithComments (Node Expression))
charLiteralExpressionOptimisticLayout =
    charLiteralExpression |> followedByOptimisticLayout


literalExpressionOptimisticLayout : Parser (WithComments (Node Expression))
literalExpressionOptimisticLayout =
    literalExpression |> followedByOptimisticLayout


listOrGlslExpressionOptimisticLayout : Parser (WithComments (Node Expression))
listOrGlslExpressionOptimisticLayout =
    listOrGlslExpression |> followedByOptimisticLayout


followedByOptimisticLayout : Parser (WithComments a) -> Parser (WithComments a)
followedByOptimisticLayout parser =
    ParserFast.map2
        (\result commentsAfter ->
            { comments = result.comments |> Rope.prependTo commentsAfter
            , syntax = result.syntax
            }
        )
        parser
        Layout.optimisticLayout


recordAccessFunctionExpressionMaybeApplied : Parser (WithComments (Node Expression))
recordAccessFunctionExpressionMaybeApplied =
    recordAccessFunctionExpression |> followedByMultiArgumentApplication


recordExpressionFollowedByRecordAccessMaybeApplied : Parser (WithComments (Node Expression))
recordExpressionFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if record access
    recordExpressionFollowedByRecordAccess
        |> followedByMultiArgumentApplication


tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied : Parser (WithComments (Node Expression))
tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if not parenthesized
    tupledExpressionIfNecessaryFollowedByRecordAccess
        |> followedByMultiArgumentApplication


caseOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (Node Expression))
caseOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        caseExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


letOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (Node Expression))
letOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        letExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


ifOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (Node Expression))
ifOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        ifBlockExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


referenceOrNumberExpressionMaybeApplied : Parser (WithComments (Node Expression))
referenceOrNumberExpressionMaybeApplied =
    ParserFast.oneOf3
        (qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (numberExpression |> followedByOptimisticLayout)


followedByMultiArgumentApplication : Parser (WithComments (Node Expression)) -> Parser (WithComments (Node Expression))
followedByMultiArgumentApplication appliedExpressionParser =
    ParserFast.map3
        (\leftExpressionResult commentsBeforeExtension maybeArgsReverse ->
            { comments =
                leftExpressionResult.comments
                    |> Rope.prependTo commentsBeforeExtension
                    |> Rope.prependTo maybeArgsReverse.comments
            , syntax =
                case maybeArgsReverse.syntax of
                    [] ->
                        leftExpressionResult.syntax

                    ((Node lastArgRange _) :: _) as argsReverse ->
                        let
                            ((Node leftRange _) as leftNode) =
                                leftExpressionResult.syntax
                        in
                        Node { start = leftRange.start, end = lastArgRange.end }
                            (Expression.Application
                                (leftNode :: List.reverse argsReverse)
                            )
            }
        )
        appliedExpressionParser
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfter ->
                        { comments = arg.comments |> Rope.prependTo commentsAfter
                        , syntax = arg.syntax
                        }
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
    , extensionRightParser : Parser (WithComments ExtensionRight)
    }


errUnknownInfixOperator : Result String a
errUnknownInfixOperator =
    Err "unknown infix operator"


infixLeft : Int -> String -> InfixOperatorInfo
infixLeft leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = Infix.Left
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence > leftPrecedence then
                        Ok rightInfo

                    else
                        temporaryErrPrecedenceTooHigh
            }
    }


infixRight : Int -> String -> InfixOperatorInfo
infixRight leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = Infix.Right
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Ok rightInfo

                    else
                        temporaryErrPrecedenceTooHigh
            }
    }


infixNonAssociative : Int -> String -> InfixOperatorInfo
infixNonAssociative leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting =
                \rightInfo ->
                    if rightInfo.leftPrecedence == leftPrecedence then
                        problemCannotMixNonAssociativeInfixOperators

                    else
                        rightInfo.extensionRightParser
            , direction = Infix.Non
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Ok rightInfo

                    else
                        temporaryErrPrecedenceTooHigh
            }
    }


type ExtensionRight
    = ExtendRightByOperation
        { symbol : String
        , direction : Infix.InfixDirection
        , expression : Node Expression
        }
