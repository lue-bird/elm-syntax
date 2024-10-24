module Elm.Parser.Expression exposing (expression, failIfDifferentFrom, functionSignatureFromVarPointer)

import Combine exposing (Parser, Step(..))
import Dict exposing (Dict)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Parser.Whitespace as Whitespace
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import List.Extra
import Parser as Core exposing ((|.), (|=), Nestable(..))
import Parser.Extra


subExpressions : Parser State (Node Expression)
subExpressions =
    Combine.lazy
        (\() ->
            Combine.oneOf
                [ referenceExpression
                , literalExpression
                , numberExpression
                , tupledExpression
                , glslExpression
                , listExpression
                , recordExpression
                , caseExpression
                , lambdaExpression
                , letExpression
                , ifBlockExpression
                , recordAccessFunctionExpression
                , negationOperation
                , charLiteralExpression
                ]
        )


andThenOneOf : List ( Int, Node Expression -> Parser State (Node Expression) )
andThenOneOf =
    -- TODO Add tests for all operators
    -- TODO Report a syntax error when encountering multiple of the comparison operators
    -- `a < b < c` is not valid Elm syntax
    [ recordAccess
    , infixLeft 1 "|>"
    , infixRight 5 "++"
    , infixRight 1 "<|"
    , infixRight 9 ">>"
    , infixNonAssociative 4 "=="
    , infixLeft 7 "*"
    , infixRight 5 "::"
    , infixLeft 6 "+"
    , infixLeftSubtraction 6
    , infixLeft 6 "|."
    , infixRight 3 "&&"
    , infixLeft 5 "|="
    , infixLeft 9 "<<"
    , infixNonAssociative 4 "/="
    , infixLeft 7 "//"
    , infixLeft 7 "/"
    , infixRight 7 "</>"
    , infixRight 2 "||"
    , infixNonAssociative 4 "<="
    , infixNonAssociative 4 ">="
    , infixNonAssociative 4 ">"
    , infixLeft 8 "<?>"
    , infixNonAssociative 4 "<"
    , infixRight 8 "^"

    -- function application must be last
    -- TODO validate function application arguments (issue #209)
    , functionCall
    ]


expression : Parser State (Node Expression)
expression =
    subExpression 0


recordAccess : ( Int, Node Expression -> Parser State (Node Expression) )
recordAccess =
    postfix 100
        recordAccessParser
        (\((Node { start } _) as left) ((Node { end } _) as field) ->
            Node
                { start = start, end = end }
                (Expression.RecordAccess left field)
        )


recordAccessParser : Parser State (Node String)
recordAccessParser =
    Core.succeed (\offset -> \source -> String.slice (offset - 1) offset source)
        |= Core.getOffset
        |= Core.getSource
        |> Core.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    Core.problem "Record access can't start with a space"

                else
                    Core.succeed identity
                        |. Tokens.dot
                        |= Node.parserCore Tokens.functionName
            )
        |> Combine.fromCore


functionCall : ( Int, Node Expression -> Parser State (Node Expression) )
functionCall =
    infixLeftWithState 90
        Layout.positivelyIndented
        (\((Node { start } leftValue) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (case leftValue of
                    Expression.Application args ->
                        Expression.Application (args ++ [ right ])

                    _ ->
                        Expression.Application [ left, right ]
                )
        )


glslExpression : Parser State (Node Expression)
glslExpression =
    let
        start : String
        start =
            "[glsl|"

        end : String
        end =
            "|]"
    in
    Core.mapChompedString
        (\s () -> s |> String.dropLeft (String.length start) |> GLSLExpression)
        (Core.multiComment start end NotNestable)
        |. Core.symbol end
        |> Node.parserFromCore


listExpression : Parser State (Node Expression)
listExpression =
    Combine.succeed ListExpr
        |> Combine.ignoreEntirely Tokens.squareStart
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep (Combine.sepBy "," expression)
        |> Combine.ignoreEntirely Tokens.squareEnd
        |> Node.parser



-- recordExpression


recordExpression : Parser State (Node Expression)
recordExpression =
    Tokens.curlyStart
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Tokens.curlyEnd
                    |> Combine.fromCoreMap (\() -> RecordExpr [])
                , recordContents
                ]
            )
        |> Node.parser


recordContents : Parser State Expression
recordContents =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\fname ->
                Combine.oneOf
                    [ recordUpdateSyntaxParser fname
                    , Tokens.equal
                        |> Combine.continueWithFromCore expression
                        |> Combine.andThen
                            (\e ->
                                let
                                    fieldUpdate : Node RecordSetter
                                    fieldUpdate =
                                        Node.combine Tuple.pair fname e

                                    toRecordExpr : List (Node RecordSetter) -> Expression
                                    toRecordExpr fieldUpdates =
                                        RecordExpr (fieldUpdate :: fieldUpdates)
                                in
                                Combine.oneOf
                                    [ Tokens.curlyEnd
                                        |> Combine.fromCoreMap (\() -> toRecordExpr [])
                                    , Combine.succeed toRecordExpr
                                        |> Combine.ignoreEntirely Tokens.comma
                                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                                        |> Combine.keep recordFields
                                        |> Combine.ignoreEntirely Tokens.curlyEnd
                                    ]
                            )
                    ]
            )


recordUpdateSyntaxParser : Node String -> Parser State Expression
recordUpdateSyntaxParser fname =
    Combine.succeed (\e -> RecordUpdateExpression fname e)
        |> Combine.ignoreEntirely Tokens.pipe
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep recordFields
        |> Combine.ignoreEntirely Tokens.curlyEnd


recordFields : Parser State (List (Node RecordSetter))
recordFields =
    Combine.succeed (\first -> \rest -> first :: rest)
        |> Combine.keep recordField
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep
            (Combine.many
                (Tokens.comma
                    |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
                    |> Combine.continueWith recordField
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                )
            )


recordField : Parser State (Node RecordSetter)
recordField =
    Combine.succeed (\fnName -> \expr -> ( fnName, expr ))
        |> Combine.keep recordFieldWithoutValue
        |> Combine.keep expression
        |> Node.parser


recordFieldWithoutValue : Parser State (Node String)
recordFieldWithoutValue =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.equal


literalExpression : Parser State (Node Expression)
literalExpression =
    Core.oneOf
        [ Tokens.multiLineStringLiteral
        , Tokens.stringLiteral
        ]
        |> Core.map Literal
        |> Node.parserFromCore


charLiteralExpression : Parser State (Node Expression)
charLiteralExpression =
    Tokens.characterLiteral
        |> Core.map CharLiteral
        |> Node.parserFromCore



-- lambda


lambdaExpression : Parser State (Node Expression)
lambdaExpression =
    Combine.succeed
        (\start ->
            \args ->
                \((Node { end } _) as expr) ->
                    Lambda args expr
                        |> LambdaExpression
                        |> Node { start = start, end = end }
        )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.backSlash
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep (Combine.sepBy1WithState (Combine.maybeIgnore Layout.layout) Patterns.pattern)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.arrowRight
        |> Combine.keep expression



-- Case Expression


caseExpression : Parser State (Node Expression)
caseExpression =
    Combine.succeed
        (\start ->
            \caseBlock_ ->
                \( end, cases ) ->
                    Node { start = start, end = end }
                        (CaseExpression (CaseBlock caseBlock_ cases))
        )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.caseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep expression
        |> Combine.ignore Layout.positivelyIndented
        |> Combine.ignoreEntirely Tokens.ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState caseStatements)


caseStatements : Parser State ( Location, Cases )
caseStatements =
    Combine.many1WithEndLocationForLastElement (\( _, Node range _ ) -> range) caseStatement


caseStatement : Parser State Case
caseStatement =
    Combine.succeed (\pattern -> \expr -> ( pattern, expr ))
        |> Combine.ignore Layout.onTopIndentation
        |> Combine.keep Patterns.pattern
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.arrowRight
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep expression



-- Let Expression


letExpression : Parser State (Node Expression)
letExpression =
    withIndentedState
        (Combine.succeed
            (\start ->
                \declarations ->
                    \((Node { end } _) as expr) ->
                        Node { start = start, end = end }
                            (LetExpression (LetBlock declarations expr))
            )
            |> Combine.keepFromCore Parser.Extra.location
            |> Combine.ignoreEntirely Tokens.letToken
            |> Combine.ignore Layout.layout
            |> Combine.keep (withIndentedState letDeclarations)
            |> Combine.ignore Layout.optimisticLayout
            |> Combine.ignoreEntirely Tokens.inToken
        )
        |> Combine.keep expression


letDeclarations : Parser State (List (Node LetDeclaration))
letDeclarations =
    Combine.many1 blockElement


blockElement : Parser State (Node LetDeclaration)
blockElement =
    Layout.onTopIndentation
        |> Combine.continueWith Patterns.pattern
        |> Combine.andThen
            (\(Node r p) ->
                case p of
                    Pattern.VarPattern v ->
                        functionWithNameNode (Node r v)
                            |> Combine.map (\fn -> Node (Expression.functionRange fn) (LetFunction fn))

                    _ ->
                        letDestructuringDeclarationWithPattern (Node r p)
            )


letDestructuringDeclarationWithPattern : Node Pattern -> Parser State (Node LetDeclaration)
letDestructuringDeclarationWithPattern ((Node { start } _) as pattern) =
    Combine.succeed
        (\((Node { end } _) as expr) ->
            Node { start = start, end = end } (LetDestructuring pattern expr)
        )
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.keep expression


numberExpression : Parser State (Node Expression)
numberExpression =
    Node.parserFromCore (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)


ifBlockExpression : Parser State (Node Expression)
ifBlockExpression =
    Combine.succeed
        (\start ->
            \condition ->
                \ifTrue ->
                    \((Node { end } _) as ifFalse) ->
                        Node
                            { start = start, end = end }
                            (IfBlock condition ifTrue ifFalse)
        )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.ifToken
        |> Combine.keep expression
        |> Combine.ignoreEntirely Tokens.thenToken
        |> Combine.keep expression
        |> Combine.ignoreEntirely Tokens.elseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep expression


negationOperation : Parser State (Node Expression)
negationOperation =
    Combine.succeed
        (\((Node { start, end } _) as subExpr) ->
            Node
                { start = { row = start.row, column = start.column - 1 }, end = end }
                (Negation subExpr)
        )
        |> Combine.ignoreEntirely minusNotFollowedBySpace
        |> Combine.keep (subExpression 95)


minusNotFollowedBySpace : Core.Parser ()
minusNotFollowedBySpace =
    Core.succeed identity
        |. Core.backtrackable Tokens.minus
        |= Core.oneOf
            [ Core.map (always True) (Core.backtrackable Whitespace.realNewLine)
            , Core.map (always True) (Core.backtrackable (Core.symbol " "))
            , Core.succeed False
            ]
        |> Core.andThen
            (\isSpaceOrComment ->
                if isSpaceOrComment then
                    Core.problem "negation sign cannot be followed by a space"

                else
                    Core.commit ()
            )


referenceExpression : Parser State (Node Expression)
referenceExpression =
    let
        helper : ModuleName -> String -> Core.Parser Expression
        helper moduleNameSoFar nameOrSegment =
            Core.oneOf
                [ Core.succeed identity
                    |. Tokens.dot
                    |= Core.oneOf
                        [ Tokens.typeName
                            |> Core.andThen (\t -> helper (nameOrSegment :: moduleNameSoFar) t)
                        , Tokens.functionName
                            |> Core.map
                                (\name ->
                                    FunctionOrValue
                                        (List.reverse (nameOrSegment :: moduleNameSoFar))
                                        name
                                )
                        ]
                , Core.succeed ()
                    |> Core.map (\() -> FunctionOrValue (List.reverse moduleNameSoFar) nameOrSegment)
                ]
    in
    Core.oneOf
        [ Tokens.typeName
            |> Core.andThen (\t -> helper [] t)
        , Tokens.functionName
            |> Core.map (\v -> FunctionOrValue [] v)
        ]
        |> Node.parserFromCore


recordAccessFunctionExpression : Parser State (Node Expression)
recordAccessFunctionExpression =
    Core.succeed (\field -> RecordAccessFunction ("." ++ field))
        |. Tokens.dot
        |= Tokens.functionName
        |> Node.parserFromCore


tupledExpression : Parser State (Node Expression)
tupledExpression =
    let
        commaSep : Parser State (List (Node Expression))
        commaSep =
            Combine.many
                (Tokens.comma
                    |> Combine.continueFromCore expression
                )

        nested : Parser State Expression
        nested =
            Combine.succeed asExpression
                |> Combine.keep expression
                |> Combine.keep commaSep
    in
    Tokens.parensStart
        |> Combine.continueFromCore
            (Combine.oneOf
                [ Tokens.parensEnd |> Combine.fromCoreMap (\() -> UnitExpr)
                , closingPrefixOperator
                , nested |> Combine.ignoreEntirely Tokens.parensEnd
                ]
            )
        |> Node.parser


closingPrefixOperator : Parser state Expression
closingPrefixOperator =
    Core.backtrackable Tokens.prefixOperatorToken
        |. Tokens.parensEnd
        |. Core.commit ()
        |> Combine.fromCoreMap PrefixOperator


asExpression : Node Expression -> List (Node Expression) -> Expression
asExpression x =
    \xs ->
        case xs of
            [] ->
                ParenthesizedExpression x

            _ ->
                TupledExpression (x :: xs)


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    Combine.withLocation
        (\location ->
            Combine.modifyState (State.pushIndent location.column)
                |> Combine.continueWith p
                |> Combine.ignore (Combine.modifyState State.popIndent)
        )


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


functionWithSignature : Node String -> Parser State Function
functionWithSignature varPointer =
    functionSignatureFromVarPointer varPointer
        |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)
        |> Combine.andThen
            (\sig ->
                Node.parserFromCore Tokens.functionName
                    |> Combine.andThen (\fnName -> failIfDifferentFrom varPointer fnName)
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    |> Combine.andThen (\newPointer -> functionImplementationFromVarPointer newPointer)
                    |> Combine.map (\decl -> fromParts sig decl)
            )


functionWithoutSignature : Node String -> Parser State Function
functionWithoutSignature varPointer =
    functionImplementationFromVarPointer varPointer
        |> Combine.map (\decl -> Function Nothing Nothing decl)


functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
functionImplementationFromVarPointer ((Node { start } _) as varPointer) =
    Combine.succeed
        (\args ->
            \((Node { end } _) as expr) ->
                Node { start = start, end = end }
                    (FunctionImplementation varPointer args expr)
        )
        |> Combine.keep (Combine.many (Patterns.pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout)))
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.keep expression


fromParts : Node Signature -> Node FunctionImplementation -> Function
fromParts sig decl =
    { documentation = Nothing
    , signature = Just sig
    , declaration = decl
    }


failIfDifferentFrom : Node String -> Node String -> Parser State (Node String)
failIfDifferentFrom (Node _ expectedName) ((Node _ actualName) as actual) =
    if expectedName == actualName then
        Combine.succeed actual

    else
        Combine.problem <| "Expected to find the declaration for " ++ expectedName ++ " but found " ++ actualName


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    Combine.succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignoreEntirely Tokens.colon
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep TypeAnnotation.typeAnnotation



---


subExpression : Int -> Parser State (Node Expression)
subExpression currentPrecedence =
    let
        parser : Node Expression -> Parser State (Step (Node Expression) (Node Expression))
        parser =
            expressionHelp currentPrecedence
    in
    spacesAndSubExpressions
        |> Combine.andThen
            (\leftExpression -> Combine.loop leftExpression parser)


spacesAndSubExpressions : Parser State (Node Expression)
spacesAndSubExpressions =
    Layout.optimisticLayout
        |> Combine.continueWith subExpressions


expressionHelp : Int -> Node Expression -> Parser State (Step (Node Expression) (Node Expression))
expressionHelp currentPrecedence leftExpression =
    case Dict.get currentPrecedence operations of
        Just parser ->
            Layout.optimisticLayout
                |> Combine.continueWith
                    (Combine.oneOf
                        [ parser leftExpression
                            |> Combine.map Loop
                        , Combine.succeed (Done leftExpression)
                        ]
                    )

        Nothing ->
            Combine.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


operation : Int -> Node Expression -> Parser State (Node Expression)
operation currentPrecedence leftExpression =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just (parser leftExpression)

                else
                    Nothing
            )
        |> Combine.oneOf


operations : Dict Int (Node Expression -> Parser State (Node Expression))
operations =
    andThenOneOf
        |> List.map Tuple.first
        |> List.Extra.unique
        |> (::) 0
        |> (::) 95
        |> List.foldl
            (\precedence dict -> Dict.insert precedence (operation precedence) dict)
            Dict.empty


infixLeft : Int -> String -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeft precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> ( Int, Node Expression -> Parser State (Node Expression) )
infixNonAssociative precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> ( Int, Node Expression -> Parser State (Node Expression) )
infixRight precedence symbol =
    infixRightHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Right left right)
        )


infixLeftSubtraction : Int -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeftSubtraction precedence =
    infixLeftHelp precedence
        (Core.succeed (\offset -> \source -> String.slice (offset - 1) offset source)
            |= Core.getOffset
            |= Core.getSource
            |> Core.andThen
                (\c ->
                    -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                    if c == " " || c == "\n" || c == "\u{000D}" then
                        Tokens.minusSymbols

                    else
                        Tokens.minus
                )
        )
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication "-" Infix.Left left right)
        )


infixLeftHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeftHelp precedence p apply =
    infixHelp precedence precedence p apply


infixLeftWithState : Int -> Parser State () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeftWithState precedence operator apply =
    let
        parser : Parser State (Node Expression)
        parser =
            subExpression precedence
    in
    ( precedence
    , \left ->
        Combine.succeed (\e -> apply left e)
            |> Combine.ignore operator
            |> Combine.keep parser
    )


infixRightHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixRightHelp precedence p apply =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply


infixHelp : Int -> Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixHelp leftPrecedence rightPrecedence operator apply =
    let
        parser : Parser State (Node Expression)
        parser =
            subExpression rightPrecedence
    in
    ( leftPrecedence
    , \left ->
        Combine.succeed (\e -> apply left e)
            |> Combine.ignoreEntirely operator
            |> Combine.keep parser
    )


postfix : Int -> Parser state a -> (expr -> a -> expr) -> ( Int, expr -> Parser state expr )
postfix precedence operator apply =
    ( precedence
    , \left -> Combine.map (\right -> apply left right) operator
    )
