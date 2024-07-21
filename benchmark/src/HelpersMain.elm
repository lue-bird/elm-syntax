module HelpersMain exposing (main)

import Benchmark
import Benchmark.Alternative
import Benchmark.Runner
import Benchmark.Runner.Alternative
import Combine
import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Parser.Whitespace as Whitespace
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Hex
import Html exposing (th)
import List.Extra
import Parser as Core exposing ((|.), (|=), Nestable(..), Parser, Step(..))
import Parser.Advanced
import Parser.Extra
import Set exposing (Set)
import Unicode


main : Benchmark.Runner.Alternative.Program
main =
    Benchmark.describe "helpers"
        [ Benchmark.Alternative.rank "List reverse String concat"
            (\f -> f exampleListOfStrings)
            [ ( "List.reverse |> String.concat"
              , listReverseStringConcatUsingListReverseStringConcatUsing
              )
            , ( "recursive ++"
              , listReverseStringConcatUsingRecursive
              )
            ]
        , Benchmark.Alternative.rank "string literal parse"
            (\stringLiteralParse -> List.map (Core.run stringLiteralParse) exampleListOfStrings)
            [ ( "loop :: |> listReverseStringConcat", stringLiteralUsingConsReverse )
            , ( "loop ++ after", stringLiteralUsingAppendAfter )
            ]
        , Benchmark.Alternative.rank "minusSymbols"
            (\minusSymbols -> List.map (Core.run minusSymbols) exampleMinusSymbols)
            [ ( "shared symbol in each branch", minusSymbolsUsingSharedSymbolInEachBranch )
            , ( "shared symbol then each branch", minusSymbolsUsingSharedSymbolThenEachBranch )
            ]
        , Benchmark.Alternative.rank "escapedCharValue"
            (\escapedCharValueParser -> List.map (Core.run escapedCharValueParser) exampleEscapedCharValueList)
            [ ( "map (\\() -> )", escapedCharValueUsingMapLazy )
            , ( "succeed |.", escapedCharValueUsingSucceedIgnore )
            ]
        , Benchmark.Alternative.rank "uppercase name with .s"
            (\escapedCharValueParser -> List.map (Core.run escapedCharValueParser) exampleListOfUppercaseNames)
            [ ( "sep1 (loop)", uppercaseNameUsingSep1 )
            , ( "recursive andThen giving head and tail", uppercaseNameUsingRecursiveAndThen )
            , ( "recursive andThen giving emptiable", uppercaseNameUsingRecursiveAndThenUsingEmptiable )
            , ( "loop with state emptiable", uppercaseNameUsingLoopUsingEmptiable )
            , ( "recursive andThen giving emptiable without needing reverse", uppercaseNameUsingRecursiveAndThenUsingEmptiableWithoutReverse )
            , ( "recursive lazy giving emptiable without needing reverse", uppercaseNameUsingRecursiveLazyUsingEmptiableWithoutReverse )
            , ( "loop with state string |> String.split", uppercaseNameUsingLoopUsingStringAndFinalSplit )
            ]
        , Benchmark.Alternative.rank "import definition"
            (\importDefinitionParser -> List.map (Core.run (importDefinitionParser |> (\(Combine.Parser p) -> p State.emptyState))) exampleListOfImportDefinitions)
            [ ( "andThens", importDefinitionUsingAndThens )
            , ( "keeps and ignores", importDefinitionUsingAndThens )
            ]
        , Benchmark.Alternative.rank "Combine.many (with generics list as reference)"
            (\importDefinitionParser -> List.map (Core.run (importDefinitionParser |> (\(Combine.Parser p) -> p State.emptyState))) exampleListOfGenericLists)
            [ ( "loop reverse", genericListUsingLoopReverse )
            , ( "recursive andThen", genericListUsingRecursiveAndThen )
            ]
        , Benchmark.Alternative.rank "operation expression"
            (\expressionParser -> List.map (Core.run (expressionParser |> (\(Combine.Parser p) -> p State.emptyState))) exampleExpressionList)
            [ ( "getOperation as case on predefined", expressionUsingGetOperationCaseToPredefined )
            , ( "getOperation as case on direct", expressionUsingGetOperationCaseToDirect )
            , ( "getOperation as case on lambda", expressionUsingGetOperationCaseToLambda )
            , ( "getOperation as case on predefined, early filterMap + saving lazy on sub-expressions", expressionUsingGetOperationCaseToPredefinedEarlyFilterMapAndSavingLazyOnSubExpressions )
            , ( "getOperation as case on predefined, early filterMap", expressionUsingGetOperationCaseToPredefinedEarlyFilterMap )
            ]
        , Benchmark.Alternative.rank "realNewLine"
            (\realNewLine -> List.map (Core.run realNewLine) exampleRealNewLineList)
            [ ( "oneOf [chompIf, succeed] |. chompIf", realNewLineUsingChompIfAndSharedChompIf )
            , ( "oneOf [chompIf, succeed] |. symbol", realNewLineUsingChompIfAndSharedSymbol )
            , ( "oneOf [symbol, succeed] |. symbol", realNewLineUsingSymbolAndSharedSymbol )
            , ( "oneOf [symbol, symbol]", realNewLineUsingSymbol )
            , ( "oneOf [symbol, chompIf]", realNewLineUsingSymbolChompIf )
            ]
        , Benchmark.Alternative.rank "as |. layout"
            (\asThenLayout -> List.map (Core.run (asThenLayout |> (\(Combine.Parser p) -> p State.emptyState))) exampleAsWordList)
            [ ( "token", asTokenAndLayout )
            , ( "keyword", asKeywordAndLayout )
            , ( "symbol (alias of token)", asSymbolAndLayout )
            ]
        , Benchmark.Alternative.rank "string literal parse"
            (\stringLiteralParse -> List.map (Core.run stringLiteralParse) exampleListOfStrings)
            [ ( "loop :: |> listReverseStringConcat", stringLiteralUsingConsReverse )
            , ( "loop ++ after", stringLiteralUsingAppendAfter )
            ]
        , Benchmark.Alternative.rank "variable name with reserved words parse"
            (\variableNameParse -> List.map (Core.run variableNameParse) exampleListOfVariableNames)
            [ ( "reserved words by case + Char.isAlphaNum shortcut", functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcut )
            , ( "reserved words by case check + Char.isAlphaNum shortcut", functionNameUsingReservedWordCaseCheckAndCharIsAlphaNumShortcut )
            , ( "reserved words by comparisons + Char.isAlphaNum shortcut", functionNameUsingReservedWordComparisonsAndCharIsAlphaNumShortcut )
            , ( "reserved words by oneOf based on first char + Char.isAlphaNum shortcut", functionNameUsingReservedWordOneOfBasedOnFirstCharAndCharIsAlphaNumShortcut )
            , ( "reserved words as Set + Char.isAlphaNum shortcut", functionNameUsingReservedWordSetAndCharIsAlphaNumShortcut )
            , ( "reserved words by oneOf + Char.isAlphaNum shortcut", functionNameUsingReservedWordOneOfAndCharIsAlphaNumShortcut )
            , ( "reserved words by case + Char.isAlphaNum shortcut + custom variable parse logic", functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcutAndCustomLogic )
            , ( "reserved words by case + Char.isAlphaNum shortcut + variable parse logic with variable only for first", functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcutAndLogicWithVariableOnlyForFirst )
            , ( "reserved words by comparisons", functionNameUsingReservedWordComparisons )
            , ( "reserved words as Set", functionNameUsingReservedWordSet )
            , ( "reserved words by case", functionNameUsingReservedWordCase )
            ]
        , Benchmark.Alternative.rank
            "one or more spaces"
            (\oneOrMoreSpacesParser -> List.map (Core.run oneOrMoreSpacesParser) exampleSpacesList)
            [ ( "Core.variable { start == ' ' inner == ' ' }", oneOrMoreSpacesUsingVariableEquals )
            , ( "Core.variable { start case == ' ' inner case == ' ' }", oneOrMoreSpacesUsingVariableCase )
            , ( "symbol ' ' |. chompWhile == ' '", oneOrMoreSpacesUsingSymbolThenChompWhileEquals )
            , ( "symbol ' ' |. chompWhile case == ' '", oneOrMoreSpacesUsingSymbolThenChompWhileCase )
            ]
        ]
        |> Benchmark.Runner.Alternative.program


exampleSpacesList : List String
exampleSpacesList =
    List.range 0 50 |> List.map (\n -> String.repeat n " ")


oneOrMoreSpacesUsingSymbolThenChompWhileEquals : Parser ()
oneOrMoreSpacesUsingSymbolThenChompWhileEquals =
    Core.token " "
        |. Core.chompWhile (\c -> c == ' ')


oneOrMoreSpacesUsingSymbolThenChompWhileCase : Parser ()
oneOrMoreSpacesUsingSymbolThenChompWhileCase =
    Core.token " "
        |. Core.chompWhile
            (\c ->
                case c of
                    ' ' ->
                        True

                    _ ->
                        False
            )


oneOrMoreSpacesUsingVariableEquals : Parser ()
oneOrMoreSpacesUsingVariableEquals =
    Core.variable
        { start = \c -> c == ' '
        , inner = \c -> c == ' '
        , reserved = Set.empty
        }
        |> Core.map (\_ -> ())


oneOrMoreSpacesUsingVariableCase : Parser ()
oneOrMoreSpacesUsingVariableCase =
    Core.variable
        { start =
            \c ->
                case c of
                    ' ' ->
                        True

                    _ ->
                        False
        , inner =
            \c ->
                case c of
                    ' ' ->
                        True

                    _ ->
                        False
        , reserved = Set.empty
        }
        |> Core.map (\_ -> ())


exampleAsWordList : List String
exampleAsWordList =
    [ "as List", "exposing  ", "as\n  List", "as   List", "as\n  ", "exposing (Dict)", ":: ", ":: tail)", "exposing ", "exposing\n (Dict)", "exposing(..)", "as \n" ]


asKeywordAndLayout : Combine.Parser State ()
asKeywordAndLayout =
    Core.keyword "as"
        |> Combine.ignoreFromCore Layout.layout


asTokenAndLayout : Combine.Parser State ()
asTokenAndLayout =
    Core.token "as"
        |> Combine.ignoreFromCore Layout.layout


asSymbolAndLayout : Combine.Parser State ()
asSymbolAndLayout =
    Core.symbol "as"
        |> Combine.ignoreFromCore Layout.layout


exampleRealNewLineList : List String
exampleRealNewLineList =
    [ "\u{000D}\n", "\u{000D}", "a", " a", "\n\n", "\n", " \n", "\n", "\n", " ", "   ", "\u{000D}\n" ]


realNewLineUsingChompIfAndSharedChompIf : Core.Parser ()
realNewLineUsingChompIfAndSharedChompIf =
    Core.oneOf
        [ Core.chompIf (\c -> c == '\u{000D}')
        , Core.succeed ()
        ]
        |. Core.chompIf (\c -> c == '\n')


realNewLineUsingChompIfAndSharedSymbol : Core.Parser ()
realNewLineUsingChompIfAndSharedSymbol =
    Core.oneOf
        [ Core.chompIf (\c -> c == '\u{000D}')
        , Core.succeed ()
        ]
        |. Core.symbol "\n"


realNewLineUsingSymbolAndSharedSymbol : Core.Parser ()
realNewLineUsingSymbolAndSharedSymbol =
    Core.oneOf
        [ Core.symbol "\u{000D}"
        , Core.succeed ()
        ]
        |. Core.symbol "\n"


realNewLineUsingSymbol : Core.Parser ()
realNewLineUsingSymbol =
    Core.oneOf
        [ Core.symbol "\u{000D}\n"
        , Core.symbol "\n"
        ]


realNewLineUsingSymbolChompIf : Core.Parser ()
realNewLineUsingSymbolChompIf =
    Core.oneOf
        [ Core.symbol "\u{000D}\n"
        , Core.chompIf (\c -> c == '\n')
        ]


exampleExpressionList : List String
exampleExpressionList =
    [ "A.B.foo.bar.baz"
    , "List.map .name people"
    , ".spaceEvenly Internal.Style.classes"
    , "1++"
    , "2-1"
    , "toFloat -5"
    , "-(x - y)"
    , "-1 + -10 * -100^2 == -100001"
    , "-1 + -10 * -99^2 == -100002"
    , "-1 + -10 / -98^2 /= -100003"
    , " 1 + 2 - 3"
    , "c == ' ' || c == '\\n' || c == '\\r'"
    , "a |> b"
    , "{ d | b = f x y }.b"
    , "1 + -{x = 10}.x"
    , "maybeErrorInfoAndFix |> Maybe.map identity |> Rule.errorWithFix e.info dotFieldRange e.fix"
    , "long |> ahh |> function |> call |> chain with arguments |> isnt |> that |> lovely |> or |> what"
    , "even |> more |> pipes |> no |> way |> List.map |> Result.andThen validate"
    , """Core.succeed String.slice
            |= Core.getOffset
            |= Core.getSource
            |> Core.andThen
            |> Tokens.minusSymbols
            |> Tokens.minus
        """
    ]


expressionUsingGetOperationCaseToPredefinedEarlyFilterMap : Combine.Parser State (Node Expression)
expressionUsingGetOperationCaseToPredefinedEarlyFilterMap =
    subExpressionUsingGetOperationCaseToPredefinedEarlyFilterMap 0


expressionUsingGetOperationCaseToPredefinedEarlyFilterMapAndSavingLazyOnSubExpressions : Combine.Parser State (Node Expression)
expressionUsingGetOperationCaseToPredefinedEarlyFilterMapAndSavingLazyOnSubExpressions =
    subExpressionUsingGetOperationCaseToPredefinedEarlyFilterMapAndSavingLazyOnSubExpressions 0


expressionUsingGetOperationCaseToPredefined : Combine.Parser State (Node Expression)
expressionUsingGetOperationCaseToPredefined =
    subExpressionUsingGetOperationCaseToPredefined 0


expressionUsingGetOperationCaseToLambda : Combine.Parser State (Node Expression)
expressionUsingGetOperationCaseToLambda =
    subExpressionUsingGetOperationCaseToLambda 0


expressionUsingGetOperationCaseToDirect : Combine.Parser State (Node Expression)
expressionUsingGetOperationCaseToDirect =
    subExpressionUsingGetOperationCaseToDirect 0


subExpressionUsingGetOperationCaseToPredefinedEarlyFilterMap : Int -> Combine.Parser State (Node Expression)
subExpressionUsingGetOperationCaseToPredefinedEarlyFilterMap currentPrecedence =
    let
        parser : Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
        parser =
            expressionHelpUsingGetOperationCaseToPredefinedEarlyFilterMap currentPrecedence
    in
    spacesAndSubExpressions
        |> Combine.andThen
            (\leftExpression -> Combine.loop leftExpression parser)


subExpressionUsingGetOperationCaseToPredefinedEarlyFilterMapAndSavingLazyOnSubExpressions : Int -> Combine.Parser State (Node Expression)
subExpressionUsingGetOperationCaseToPredefinedEarlyFilterMapAndSavingLazyOnSubExpressions currentPrecedence =
    let
        parser : Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
        parser =
            expressionHelpUsingGetOperationCaseToPredefinedEarlyFilterMap currentPrecedence
    in
    spacesAndSubExpressionsAndSavingLazyOnSubExpressions
        |> Combine.andThen
            (\leftExpression -> Combine.loop leftExpression parser)


subExpressionUsingGetOperationCaseToPredefined : Int -> Combine.Parser State (Node Expression)
subExpressionUsingGetOperationCaseToPredefined currentPrecedence =
    let
        parser : Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
        parser =
            expressionHelpUsingGetOperationCaseToPredefined currentPrecedence
    in
    spacesAndSubExpressions
        |> Combine.andThen
            (\leftExpression -> Combine.loop leftExpression parser)


subExpressionUsingGetOperationCaseToLambda : Int -> Combine.Parser State (Node Expression)
subExpressionUsingGetOperationCaseToLambda currentPrecedence =
    let
        parser : Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
        parser =
            expressionHelpUsingGetOperationCaseToLambda currentPrecedence
    in
    spacesAndSubExpressions
        |> Combine.andThen
            (\leftExpression -> Combine.loop leftExpression parser)


subExpressionUsingGetOperationCaseToDirect : Int -> Combine.Parser State (Node Expression)
subExpressionUsingGetOperationCaseToDirect currentPrecedence =
    let
        parser : Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
        parser =
            expressionHelpUsingGetOperationCaseToDirect currentPrecedence
    in
    spacesAndSubExpressions
        |> Combine.andThen
            (\leftExpression -> Combine.loop leftExpression parser)


spacesAndSubExpressions : Combine.Parser State (Node Expression)
spacesAndSubExpressions =
    Layout.optimisticLayout
        |> Combine.continueWith subExpressions


spacesAndSubExpressionsAndSavingLazyOnSubExpressions : Combine.Parser State (Node Expression)
spacesAndSubExpressionsAndSavingLazyOnSubExpressions =
    Layout.optimisticLayout
        |> Combine.continueWith subExpressionsSavingLazyOnSubExpressions


subExpressionsSavingLazyOnSubExpressions : Combine.Parser State (Node Expression)
subExpressionsSavingLazyOnSubExpressions =
    Combine.oneOf
        [ referenceExpression
        , literalExpression
        , numberExpression
        , Combine.lazy (\() -> tupledExpression)
        , glslExpression
        , Combine.lazy (\() -> listExpression)
        , Combine.lazy (\() -> recordExpression)
        , Combine.lazy (\() -> caseExpression)
        , Combine.lazy (\() -> lambdaExpression)
        , Combine.lazy (\() -> letExpression)
        , Combine.lazy (\() -> ifBlockExpression)
        , Combine.lazy (\() -> recordAccessFunctionExpression)
        , Combine.lazy (\() -> negationOperation)
        , charLiteralExpression
        ]


subExpressions : Combine.Parser State (Node Expression)
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


expressionHelpUsingGetOperationCaseToPredefinedEarlyFilterMap : Int -> Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
expressionHelpUsingGetOperationCaseToPredefinedEarlyFilterMap currentPrecedence leftExpression =
    case getOperationUsingGetOperationCaseToPredefinedEarlyFilterMap currentPrecedence of
        Just parser ->
            Layout.optimisticLayout
                |> Combine.continueWith
                    (Combine.oneOf
                        [ combineOneOfApply parser leftExpression
                            |> Combine.map Combine.Loop
                        , Combine.succeed (Combine.Done leftExpression)
                        ]
                    )

        Nothing ->
            Combine.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


expressionHelpUsingGetOperationCaseToPredefined : Int -> Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
expressionHelpUsingGetOperationCaseToPredefined currentPrecedence leftExpression =
    case getOperationUsingGetOperationCaseToPredefined currentPrecedence of
        Just parser ->
            Layout.optimisticLayout
                |> Combine.continueWith
                    (Combine.oneOf
                        [ parser leftExpression
                            |> Combine.map Combine.Loop
                        , Combine.succeed (Combine.Done leftExpression)
                        ]
                    )

        Nothing ->
            Combine.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


expressionHelpUsingGetOperationCaseToLambda : Int -> Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
expressionHelpUsingGetOperationCaseToLambda currentPrecedence leftExpression =
    case getOperationUsingGetOperationCaseToLambda currentPrecedence of
        Just parser ->
            Layout.optimisticLayout
                |> Combine.continueWith
                    (Combine.oneOf
                        [ parser leftExpression
                            |> Combine.map Combine.Loop
                        , Combine.succeed (Combine.Done leftExpression)
                        ]
                    )

        Nothing ->
            Combine.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


expressionHelpUsingGetOperationCaseToDirect : Int -> Node Expression -> Combine.Parser State (Combine.Step (Node Expression) (Node Expression))
expressionHelpUsingGetOperationCaseToDirect currentPrecedence leftExpression =
    case getOperationUsingGetOperationCaseToDirect currentPrecedence leftExpression of
        Just parser ->
            Layout.optimisticLayout
                |> Combine.continueWith
                    (Combine.oneOf
                        [ parser
                            |> Combine.map Combine.Loop
                        , Combine.succeed (Combine.Done leftExpression)
                        ]
                    )

        Nothing ->
            Combine.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


getOperationUsingGetOperationCaseToDirect : Int -> Node Expression -> Maybe (Combine.Parser State (Node Expression))
getOperationUsingGetOperationCaseToDirect precedence leftExpression =
    case precedence of
        0 ->
            Just (leftExpression |> operation 0)

        1 ->
            Just (leftExpression |> operation 1)

        2 ->
            Just (leftExpression |> operation 2)

        3 ->
            Just (leftExpression |> operation 3)

        4 ->
            Just (leftExpression |> operation 4)

        5 ->
            Just (leftExpression |> operation 5)

        6 ->
            Just (leftExpression |> operation 6)

        7 ->
            Just (leftExpression |> operation 7)

        8 ->
            Just (leftExpression |> operation 8)

        9 ->
            Just (leftExpression |> operation 9)

        90 ->
            Just (leftExpression |> operation 90)

        95 ->
            Just (leftExpression |> operation 95)

        100 ->
            Just (leftExpression |> operation 100)

        _ ->
            Nothing


getOperationUsingGetOperationCaseToLambda : Int -> Maybe (Node Expression -> Combine.Parser State (Node Expression))
getOperationUsingGetOperationCaseToLambda precedence =
    case precedence of
        0 ->
            Just (\leftExpression -> leftExpression |> operation 0)

        1 ->
            Just (\leftExpression -> leftExpression |> operation 1)

        2 ->
            Just (\leftExpression -> leftExpression |> operation 2)

        3 ->
            Just (\leftExpression -> leftExpression |> operation 3)

        4 ->
            Just (\leftExpression -> leftExpression |> operation 4)

        5 ->
            Just (\leftExpression -> leftExpression |> operation 5)

        6 ->
            Just (\leftExpression -> leftExpression |> operation 6)

        7 ->
            Just (\leftExpression -> leftExpression |> operation 7)

        8 ->
            Just (\leftExpression -> leftExpression |> operation 8)

        9 ->
            Just (\leftExpression -> leftExpression |> operation 9)

        90 ->
            Just (\leftExpression -> leftExpression |> operation 90)

        95 ->
            Just (\leftExpression -> leftExpression |> operation 95)

        100 ->
            Just (\leftExpression -> leftExpression |> operation 100)

        _ ->
            Nothing


getOperationUsingGetOperationCaseToPredefinedEarlyFilterMap : Int -> Maybe (List (Node Expression -> Combine.Parser State (Node Expression)))
getOperationUsingGetOperationCaseToPredefinedEarlyFilterMap precedence =
    case precedence of
        0 ->
            Just operation0EarlyFilterMap

        1 ->
            Just operation1EarlyFilterMap

        2 ->
            Just operation2EarlyFilterMap

        3 ->
            Just operation3EarlyFilterMap

        4 ->
            Just operation4EarlyFilterMap

        5 ->
            Just operation5EarlyFilterMap

        6 ->
            Just operation6EarlyFilterMap

        7 ->
            Just operation7EarlyFilterMap

        8 ->
            Just operation8EarlyFilterMap

        9 ->
            Just operation9EarlyFilterMap

        90 ->
            Just operation90EarlyFilterMap

        95 ->
            Just operation95EarlyFilterMap

        100 ->
            Just operation100EarlyFilterMap

        _ ->
            Nothing


operation0EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation0EarlyFilterMap =
    andThenOneOfAbovePrecedence 0


operation1EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation1EarlyFilterMap =
    andThenOneOfAbovePrecedence 1


operation2EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation2EarlyFilterMap =
    andThenOneOfAbovePrecedence 2


operation3EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation3EarlyFilterMap =
    andThenOneOfAbovePrecedence 3


operation4EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation4EarlyFilterMap =
    andThenOneOfAbovePrecedence 4


operation5EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation5EarlyFilterMap =
    andThenOneOfAbovePrecedence 5


operation6EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation6EarlyFilterMap =
    andThenOneOfAbovePrecedence 6


operation7EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation7EarlyFilterMap =
    andThenOneOfAbovePrecedence 7


operation8EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation8EarlyFilterMap =
    andThenOneOfAbovePrecedence 8


operation9EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation9EarlyFilterMap =
    andThenOneOfAbovePrecedence 9


operation90EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation90EarlyFilterMap =
    andThenOneOfAbovePrecedence 90


operation95EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation95EarlyFilterMap =
    andThenOneOfAbovePrecedence 95


operation100EarlyFilterMap : List (Node Expression -> Combine.Parser State (Node Expression))
operation100EarlyFilterMap =
    andThenOneOfAbovePrecedence 100


getOperationUsingGetOperationCaseToPredefined : Int -> Maybe (Node Expression -> Combine.Parser State (Node Expression))
getOperationUsingGetOperationCaseToPredefined precedence =
    case precedence of
        0 ->
            Just operation0

        1 ->
            Just operation1

        2 ->
            Just operation2

        3 ->
            Just operation3

        4 ->
            Just operation4

        5 ->
            Just operation5

        6 ->
            Just operation6

        7 ->
            Just operation7

        8 ->
            Just operation8

        9 ->
            Just operation9

        90 ->
            Just operation90

        95 ->
            Just operation95

        100 ->
            Just operation100

        _ ->
            Nothing


operation0 : Node Expression -> Combine.Parser State (Node Expression)
operation0 leftExpression =
    operation 0 leftExpression


operation1 : Node Expression -> Combine.Parser State (Node Expression)
operation1 leftExpression =
    operation 1 leftExpression


operation2 : Node Expression -> Combine.Parser State (Node Expression)
operation2 leftExpression =
    operation 2 leftExpression


operation3 : Node Expression -> Combine.Parser State (Node Expression)
operation3 leftExpression =
    operation 3 leftExpression


operation4 : Node Expression -> Combine.Parser State (Node Expression)
operation4 leftExpression =
    operation 4 leftExpression


operation5 : Node Expression -> Combine.Parser State (Node Expression)
operation5 leftExpression =
    operation 5 leftExpression


operation6 : Node Expression -> Combine.Parser State (Node Expression)
operation6 leftExpression =
    operation 6 leftExpression


operation7 : Node Expression -> Combine.Parser State (Node Expression)
operation7 leftExpression =
    operation 7 leftExpression


operation8 : Node Expression -> Combine.Parser State (Node Expression)
operation8 leftExpression =
    operation 8 leftExpression


operation9 : Node Expression -> Combine.Parser State (Node Expression)
operation9 leftExpression =
    operation 9 leftExpression


operation90 : Node Expression -> Combine.Parser State (Node Expression)
operation90 leftExpression =
    operation 90 leftExpression


operation95 : Node Expression -> Combine.Parser State (Node Expression)
operation95 leftExpression =
    operation 95 leftExpression


operation100 : Node Expression -> Combine.Parser State (Node Expression)
operation100 leftExpression =
    operation 100 leftExpression


operation : Int -> Node Expression -> Combine.Parser State (Node Expression)
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


andThenOneOfAbovePrecedence : Int -> List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence currentPrecedence =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just parser

                else
                    Nothing
            )


combineOneOfApply :
    List (Node Expression -> Combine.Parser State (Node Expression))
    -> Node Expression
    -> Combine.Parser State (Node Expression)
combineOneOfApply possibilitiesForCurrentPrecedence leftExpression =
    Combine.Parser <|
        \state ->
            Core.oneOf
                (List.map
                    (\parser ->
                        let
                            (Combine.Parser x) =
                                parser leftExpression
                        in
                        x state
                    )
                    possibilitiesForCurrentPrecedence
                )


andThenOneOf : List ( Int, Node Expression -> Combine.Parser State (Node Expression) )
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


recordAccess : ( Int, Node Expression -> Combine.Parser State (Node Expression) )
recordAccess =
    postfix 100
        recordAccessParser
        (\((Node { start } _) as left) ((Node { end } _) as field) ->
            Node
                { start = start, end = end }
                (Expression.RecordAccess left field)
        )


recordAccessParser : Combine.Parser State (Node String)
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


functionCall : ( Int, Node Expression -> Combine.Parser State (Node Expression) )
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


glslStart : String
glslStart =
    "[glsl|"


glslStartLength : Int
glslStartLength =
    String.length glslStart


glslEnd : String
glslEnd =
    "|]"


glslExpression : Combine.Parser State (Node Expression)
glslExpression =
    Core.mapChompedString
        (\s () -> s |> String.dropLeft glslStartLength |> GLSLExpression)
        (Core.multiComment glslStart glslEnd NotNestable)
        |. Core.symbol glslEnd
        |> Node.parserCore
        |> Combine.fromCore


listExpression : Combine.Parser State (Node Expression)
listExpression =
    Combine.succeed ListExpr
        |> Combine.ignoreEntirely Tokens.squareStart
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep (Combine.sepBy "," expressionUsingGetOperationCaseToPredefined)
        |> Combine.ignoreEntirely Tokens.squareEnd
        |> Node.parser



-- recordExpression


recordExpression : Combine.Parser State (Node Expression)
recordExpression =
    Tokens.curlyStart
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Tokens.curlyEnd
                    |> Core.map (\() -> RecordExpr [])
                    |> Combine.fromCore
                , recordContents
                ]
            )
        |> Node.parser


recordContents : Combine.Parser State Expression
recordContents =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\fname ->
                Combine.oneOf
                    [ recordUpdateSyntaxParser fname
                    , Combine.fromCore Tokens.equal
                        |> Combine.continueWith expressionUsingGetOperationCaseToPredefined
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
                                        |> Core.map (\() -> toRecordExpr [])
                                        |> Combine.fromCore
                                    , Combine.succeed toRecordExpr
                                        |> Combine.ignoreEntirely Tokens.comma
                                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                                        |> Combine.keep recordFields
                                        |> Combine.ignoreEntirely Tokens.curlyEnd
                                    ]
                            )
                    ]
            )


recordUpdateSyntaxParser : Node String -> Combine.Parser State Expression
recordUpdateSyntaxParser fname =
    Combine.succeed (\e -> RecordUpdateExpression fname e)
        |> Combine.ignoreEntirely Tokens.pipe
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep recordFields
        |> Combine.ignoreEntirely Tokens.curlyEnd


recordFields : Combine.Parser State (List (Node RecordSetter))
recordFields =
    Combine.succeed (\first -> \rest -> first :: rest)
        |> Combine.keep recordField
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep
            (Combine.many
                (Combine.fromCore Tokens.comma
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    |> Combine.continueWith recordField
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                )
            )


recordField : Combine.Parser State (Node RecordSetter)
recordField =
    Combine.succeed (\fnName -> \expr -> ( fnName, expr ))
        |> Combine.keep recordFieldWithoutValue
        |> Combine.keep expressionUsingGetOperationCaseToPredefined
        |> Node.parser


recordFieldWithoutValue : Combine.Parser State (Node String)
recordFieldWithoutValue =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.equal


literalExpression : Combine.Parser State (Node Expression)
literalExpression =
    Core.oneOf
        [ Tokens.multiLineStringLiteral
        , Tokens.stringLiteral
        ]
        |> Core.map Literal
        |> Node.parserCore
        |> Combine.fromCore


charLiteralExpression : Combine.Parser State (Node Expression)
charLiteralExpression =
    Tokens.characterLiteral
        |> Core.map CharLiteral
        |> Node.parserCore
        |> Combine.fromCore



-- lambda


lambdaExpression : Combine.Parser State (Node Expression)
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
        |> Combine.keep expressionUsingGetOperationCaseToPredefined



-- Case Expression


caseExpression : Combine.Parser State (Node Expression)
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
        |> Combine.keep expressionUsingGetOperationCaseToPredefined
        |> Combine.ignore Layout.positivelyIndented
        |> Combine.ignoreEntirely Tokens.ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState caseStatements)


caseStatements : Combine.Parser State ( Location, Cases )
caseStatements =
    Combine.many1WithEndLocationForLastElement (\( _, Node range _ ) -> range) caseStatement


caseStatement : Combine.Parser State Case
caseStatement =
    Combine.succeed (\pattern -> \expr -> ( pattern, expr ))
        |> Combine.ignore Layout.onTopIndentation
        |> Combine.keep Patterns.pattern
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.arrowRight
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep expressionUsingGetOperationCaseToPredefined



-- Let Expression


letExpression : Combine.Parser State (Node Expression)
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
        |> Combine.keep expressionUsingGetOperationCaseToPredefined


letDeclarations : Combine.Parser State (List (Node LetDeclaration))
letDeclarations =
    Combine.many1 blockElement


blockElement : Combine.Parser State (Node LetDeclaration)
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


letDestructuringDeclarationWithPattern : Node Pattern -> Combine.Parser State (Node LetDeclaration)
letDestructuringDeclarationWithPattern ((Node { start } _) as pattern) =
    Combine.succeed
        (\((Node { end } _) as expr) ->
            Node { start = start, end = end } (LetDestructuring pattern expr)
        )
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.keep expressionUsingGetOperationCaseToPredefined


numberExpression : Combine.Parser State (Node Expression)
numberExpression =
    Node.parserCore (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)
        |> Combine.fromCore


ifBlockExpression : Combine.Parser State (Node Expression)
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
        |> Combine.keep expressionUsingGetOperationCaseToPredefined
        |> Combine.ignoreEntirely Tokens.thenToken
        |> Combine.keep expressionUsingGetOperationCaseToPredefined
        |> Combine.ignoreEntirely Tokens.elseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep expressionUsingGetOperationCaseToPredefined


negationOperation : Combine.Parser State (Node Expression)
negationOperation =
    Combine.succeed
        (\((Node { start, end } _) as subExpr) ->
            Node
                { start = { row = start.row, column = start.column - 1 }, end = end }
                (Negation subExpr)
        )
        |> Combine.ignoreEntirely minusNotFollowedBySpace
        |> Combine.keep (subExpressionUsingGetOperationCaseToPredefined 95)


minusNotFollowedBySpace : Core.Parser ()
minusNotFollowedBySpace =
    Core.succeed identity
        |. Core.backtrackable Tokens.minus
        |= Core.oneOf
            [ Core.map (\() -> True) (Core.backtrackable Whitespace.realNewLine)
            , Core.map (\() -> True) (Core.backtrackable (Core.symbol " "))
            , Core.succeed False
            ]
        |> Core.andThen
            (\isSpaceOrComment ->
                if isSpaceOrComment then
                    Core.problem "negation sign cannot be followed by a space"

                else
                    Core.commit ()
            )


referenceExpression : Combine.Parser State (Node Expression)
referenceExpression =
    Core.oneOf
        [ Tokens.typeName
            |> Core.andThen (\t -> referenceExpressionHelper [] t)
        , Tokens.functionName
            |> Core.map (\v -> FunctionOrValue [] v)
        ]
        |> Node.parserCore
        |> Combine.fromCore


referenceExpressionHelper : ModuleName -> String -> Core.Parser Expression
referenceExpressionHelper moduleNameSoFar nameOrSegment =
    Core.oneOf
        [ Core.succeed identity
            |. Tokens.dot
            |= Core.oneOf
                [ Tokens.typeName
                    |> Core.andThen (\t -> referenceExpressionHelper (nameOrSegment :: moduleNameSoFar) t)
                , Tokens.functionName
                    |> Core.map
                        (\name ->
                            FunctionOrValue
                                (List.reverse (nameOrSegment :: moduleNameSoFar))
                                name
                        )
                ]
        , Core.lazy
            (\() -> Core.succeed (FunctionOrValue (List.reverse moduleNameSoFar) nameOrSegment))
        ]


recordAccessFunctionExpression : Combine.Parser State (Node Expression)
recordAccessFunctionExpression =
    Core.succeed (\field -> RecordAccessFunction ("." ++ field))
        |. Tokens.dot
        |= Tokens.functionName
        |> Node.parserFromCore


tupledExpression : Combine.Parser State (Node Expression)
tupledExpression =
    Tokens.parensStart
        |> Combine.continueFromCore
            (Combine.oneOf
                [ Tokens.parensEnd |> Core.map (\() -> UnitExpr) |> Combine.fromCore
                , closingPrefixOperator
                , tupledExpressionInnerNested |> Combine.ignoreEntirely Tokens.parensEnd
                ]
            )
        |> Node.parser


tupledExpressionInnerCommaSep : Combine.Parser State (List (Node Expression))
tupledExpressionInnerCommaSep =
    Combine.many
        (Tokens.comma
            |> Combine.continueFromCore expressionUsingGetOperationCaseToPredefined
        )


tupledExpressionInnerNested : Combine.Parser State Expression
tupledExpressionInnerNested =
    Combine.succeed asExpression
        |> Combine.keep expressionUsingGetOperationCaseToPredefined
        |> Combine.keep tupledExpressionInnerCommaSep


closingPrefixOperator : Combine.Parser state Expression
closingPrefixOperator =
    Core.backtrackable Tokens.prefixOperatorToken
        |. Tokens.parensEnd
        |. Core.commit ()
        |> Core.map PrefixOperator
        |> Combine.fromCore


asExpression : Node Expression -> List (Node Expression) -> Expression
asExpression x =
    \xs ->
        case xs of
            [] ->
                ParenthesizedExpression x

            _ ->
                TupledExpression (x :: xs)


withIndentedState : Combine.Parser State a -> Combine.Parser State a
withIndentedState p =
    Combine.withLocation
        (\location ->
            Combine.modifyState (State.pushIndent location.column)
                |> Combine.continueWith p
                |> Combine.ignore (Combine.modifyState State.popIndent)
        )


functionWithNameNode : Node String -> Combine.Parser State Function
functionWithNameNode pointer =
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


functionWithSignature : Node String -> Combine.Parser State Function
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


functionWithoutSignature : Node String -> Combine.Parser State Function
functionWithoutSignature varPointer =
    functionImplementationFromVarPointer varPointer
        |> Combine.map (\decl -> Function Nothing Nothing decl)


functionImplementationFromVarPointer : Node String -> Combine.Parser State (Node FunctionImplementation)
functionImplementationFromVarPointer ((Node { start } _) as varPointer) =
    Combine.succeed
        (\args ->
            \((Node { end } _) as expr) ->
                Node { start = start, end = end }
                    (FunctionImplementation varPointer args expr)
        )
        |> Combine.keep (Combine.many (Patterns.pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout)))
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.keep expressionUsingGetOperationCaseToPredefined


fromParts : Node Signature -> Node FunctionImplementation -> Function
fromParts sig decl =
    { documentation = Nothing
    , signature = Just sig
    , declaration = decl
    }


failIfDifferentFrom : Node String -> Node String -> Combine.Parser State (Node String)
failIfDifferentFrom (Node _ expectedName) ((Node _ actualName) as actual) =
    if expectedName == actualName then
        Combine.succeed actual

    else
        Combine.problem <| "Expected to find the declaration for " ++ expectedName ++ " but found " ++ actualName


functionSignatureFromVarPointer : Node String -> Combine.Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    Combine.succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignoreEntirely Tokens.colon
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep TypeAnnotation.typeAnnotation


infixLeft : Int -> String -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
infixLeft precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
infixNonAssociative precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
infixRight precedence symbol =
    infixRightHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Right left right)
        )


infixLeftSubtraction : Int -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
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


infixLeftHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
infixLeftHelp precedence p apply =
    infixHelp precedence precedence p apply


infixLeftWithState : Int -> Combine.Parser State () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
infixLeftWithState precedence operator apply =
    let
        parser : Combine.Parser State (Node Expression)
        parser =
            subExpressionUsingGetOperationCaseToPredefined precedence
    in
    ( precedence
    , \left ->
        Combine.succeed (\e -> apply left e)
            |> Combine.ignore operator
            |> Combine.keep parser
    )


infixRightHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
infixRightHelp precedence p apply =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply


infixHelp : Int -> Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Combine.Parser State (Node Expression) )
infixHelp leftPrecedence rightPrecedence operator apply =
    let
        parser : Combine.Parser State (Node Expression)
        parser =
            subExpressionUsingGetOperationCaseToPredefined rightPrecedence
    in
    ( leftPrecedence
    , \left ->
        Combine.succeed (\e -> apply left e)
            |> Combine.ignoreEntirely operator
            |> Combine.keep parser
    )


postfix : Int -> Combine.Parser state a -> (expr -> a -> expr) -> ( Int, expr -> Combine.Parser state expr )
postfix precedence operator apply =
    ( precedence
    , \left -> Combine.map (\right -> apply left right) operator
    )


exampleListOfGenericLists : List String
exampleListOfGenericLists =
    [ "a"
    , "b"
    , "comparable"
    , "msg"
    , "event"
    , "future"
    , "s t a b"
    , "c"
    , "x"
    , "error"
    , "sides units measure"
    , "number"
    , "edges node info"
    , "l f m a u i k"
    , "r e e"
    , "e"
    , "l m n o p"
    , "c"
    , "d"
    , "inner outer wrap renew"
    , "id kind"
    , "key value"
    , "small or  what are  you staring at"
    ]


genericListUsingRecursiveAndThen : Combine.Parser State (List (Node String))
genericListUsingRecursiveAndThen =
    combineManyUsingRecursiveAndThen
        (Node.parserCore Tokens.functionName
            |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        )


combineManyUsingRecursiveAndThen : Combine.Parser a res -> Combine.Parser a (List res)
combineManyUsingRecursiveAndThen (Combine.Parser p) =
    Combine.Parser <| \state -> combineManyUsingRecursiveAndThenHelp p state


combineManyUsingRecursiveAndThenHelp : (state -> Core.Parser ( state, a )) -> state -> Core.Parser ( state, List a )
combineManyUsingRecursiveAndThenHelp p =
    \state ->
        Core.oneOf
            [ p state
                |> Core.andThen
                    (\( stateAfterHead, head ) ->
                        Core.map (\( stateAfterTail, tail ) -> ( stateAfterTail, head :: tail ))
                            (combineManyUsingRecursiveAndThenHelp p stateAfterHead)
                    )
            , Core.succeed ( state, [] )
            ]


genericListUsingLoopReverse : Combine.Parser State (List (Node String))
genericListUsingLoopReverse =
    combineManyUsingLoopReverse
        (Node.parserCore Tokens.functionName
            |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        )


combineManyUsingLoopReverse : Combine.Parser state a -> Combine.Parser state (List a)
combineManyUsingLoopReverse p =
    combineManyWithoutReverseUsingLoop [] p
        |> Combine.map List.reverse


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.
-}
combineManyWithoutReverseUsingLoop : List a -> Combine.Parser state a -> Combine.Parser state (List a)
combineManyWithoutReverseUsingLoop initList (Combine.Parser p) =
    let
        combineManyWithoutReverseUsingLoopStep : ( state, List a ) -> Core.Parser (Core.Step ( state, List a ) ( state, List a ))
        combineManyWithoutReverseUsingLoopStep (( oldState, items ) as acc) =
            Core.oneOf
                [ p oldState
                    |> Core.map (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                , Core.succeed (Core.Done acc)
                ]
    in
    Combine.Parser <|
        \state ->
            Core.loop ( state, initList ) combineManyWithoutReverseUsingLoopStep


exampleListOfImportDefinitions : List String
exampleListOfImportDefinitions =
    [ "import Foo exposing(Model,  Msg(..))"
    , "import Html exposing (text)"
    , "import Foo"
    , "import Boo"
    , "import Roo"
    , "import Boo.Galloon"
    , "import Foo as Bar"
    , "import Baz exposing   (..)"
    , "import Baz exposing (moe)"
    , "import Moo as  Mii exposing (moe)"
    , "import Moo as Mii exposing (..)"
    , "import  List.Extra as List"
    , "import Html.Attributes  as HA"
    , "import Html.Attributes  as HA exposing(HA)"
    ]


importDefinitionUsingAndThens : Combine.Parser State (Node Import)
importDefinitionUsingAndThens =
    Combine.succeed (\start -> \mod -> importInnerParseAsDefinitionUsingAndThens start mod)
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.importToken
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore Elm.Parser.Base.moduleName
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.andThen identity
        |> Combine.ignore Layout.optimisticLayout


importInnerAsDefinitionUsingAndThens : Combine.Parser State (Node ModuleName)
importInnerAsDefinitionUsingAndThens =
    Tokens.asToken
        |> Combine.ignoreFromCore Layout.layout
        |> Combine.continueWithCore Elm.Parser.Base.moduleName


importInnerParseExposingDefinitionUsingAndThens : Node ModuleName -> Maybe (Node ModuleName) -> Combine.Parser State Import
importInnerParseExposingDefinitionUsingAndThens mod asDef =
    Combine.oneOf
        [ Node.parser Elm.Parser.Expose.exposeDefinition
            |> Combine.map (\exposing_ -> { moduleName = mod, moduleAlias = asDef, exposingList = Just exposing_ })
        , Combine.succeed { moduleName = mod, moduleAlias = asDef, exposingList = Nothing }
        ]


importInnerParseAsDefinitionUsingAndThens : Location -> Node ModuleName -> Combine.Parser State (Node Import)
importInnerParseAsDefinitionUsingAndThens start mod =
    Combine.oneOf
        [ importInnerAsDefinitionUsingAndThens
            |> Combine.ignore Layout.optimisticLayout
            |> Combine.andThen (\alias_ -> importInnerParseExposingDefinitionUsingAndThens mod (Just alias_))
        , importInnerParseExposingDefinitionUsingAndThens mod Nothing
        ]
        |> Combine.map (\imp -> setupNode start imp)


importDefinitionUsingKeepIgnore : Combine.Parser State (Node Import)
importDefinitionUsingKeepIgnore =
    Combine.succeed
        (\start ->
            \mod ->
                \asDef ->
                    \exposingList ->
                        setupNode start { moduleName = mod, moduleAlias = asDef, exposingList = exposingList }
        )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.importToken
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore Elm.Parser.Base.moduleName
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.keep importInnerParseAsDefinitionUsingKeepIgnore
        |> Combine.keep importInnerParseExposingDefinitionUsingKeepIgnore


importInnerParseExposingDefinitionUsingKeepIgnore : Combine.Parser State (Maybe (Node Exposing))
importInnerParseExposingDefinitionUsingKeepIgnore =
    Combine.oneOf
        [ Combine.succeed Just
            |> Combine.keep (Node.parser Elm.Parser.Expose.exposeDefinition)
            |> Combine.ignore Layout.optimisticLayout
        , Combine.succeed Nothing
        ]


importInnerParseAsDefinitionUsingKeepIgnore : Combine.Parser State (Maybe (Node ModuleName))
importInnerParseAsDefinitionUsingKeepIgnore =
    Combine.oneOf
        [ Combine.succeed Just
            |> Combine.ignoreEntirely Tokens.asToken
            |> Combine.ignore Layout.layout
            |> Combine.keepFromCore Elm.Parser.Base.moduleName
        , Combine.succeed Nothing
        ]


setupNode : Location -> Import -> Node Import
setupNode start imp =
    let
        endRange : Range
        endRange =
            case imp.moduleAlias of
                Just (Node range _) ->
                    range

                Nothing ->
                    case imp.exposingList of
                        Just (Node range _) ->
                            range

                        Nothing ->
                            Node.range imp.moduleName
    in
    Node
        { start = start, end = endRange.end }
        imp


exampleListOfUppercaseNames : List String
exampleListOfUppercaseNames =
    [ "List"
    , "Sub"
    , "Cmd"
    , "List.Extra"
    , "Element.Events"
    , "Fruits.Com.Ui.Design.Language.Fire.Swish.Branding.Amazing.So.Consistent"
    , "Ui.Button.Icon"
    , "Elm.Syntax.Expression"
    , "Program"
    , "Array"
    , "Dict"
    , "Set"
    , "List"
    , "List"
    , "Html.Attributes.Attribute"
    , "Context"
    , "User"
    , "State"
    , "Event"
    , "Effect"
    , "Float"
    , "Int"
    , "Interface"
    , "Page.Home.Panel.DashboardSidebar"
    , "Page.Home.Panel.Status"
    , "Page.Home.Panel.Notifications"
    , "Page.Home.Panel.SettingsButton"
    , "Page.Home.Panel.ProfileButton"
    , "Page.Home.Panel.WelcomeMessage"
    , "Page.Home.Panel.LogOutButton"
    , "Page.Home.Main.Search"
    , "Page.Home.Main.LatestSearches"
    , "Page.Home.Main.ModeSelect"
    ]
        ++ [ "Basics.identity"
           , "String.fromInt"
           , "Basics.toFloat"
           , "Dict.map"
           , "Page.Home.Main.LatestSearches.withOptions"
           , "small"
           , "or"
           , "what"
           , "don't"
           , "look"
           , "hey"
           , "what"
           , "are"
           , "you"
           , "staring"
           , "at"
           , "{}"
           , "()"
           , "4"
           ]


uppercaseNameUsingRecursiveLazyUsingEmptiableWithoutReverse : Core.Parser (Node ModuleName)
uppercaseNameUsingRecursiveLazyUsingEmptiableWithoutReverse =
    Core.succeed listCons
        |= Tokens.typeName
        |= uppercaseNameUsingRecursiveLazyUsingEmptiableWithoutReverseHelper
        |> Node.parserCore


listCons : a -> List a -> List a
listCons head =
    \tail -> head :: tail


uppercaseNameUsingRecursiveLazyUsingEmptiableWithoutReverseHelper : Core.Parser ModuleName
uppercaseNameUsingRecursiveLazyUsingEmptiableWithoutReverseHelper =
    Core.oneOf
        [ Core.succeed listCons
            |. Tokens.dot
            |= Tokens.typeName
            |= Core.lazy (\() -> uppercaseNameUsingRecursiveLazyUsingEmptiableWithoutReverseHelper)
        , Core.succeed []
        ]


uppercaseNameUsingRecursiveAndThenUsingEmptiableWithoutReverse : Core.Parser (Node ModuleName)
uppercaseNameUsingRecursiveAndThenUsingEmptiableWithoutReverse =
    Tokens.typeName
        |> Core.andThen
            (\typeOrSegment ->
                Core.map (\further -> typeOrSegment :: further)
                    uppercaseNameUsingRecursiveAndThenUsingEmptiableWithoutReverseHelper
            )
        |> Node.parserCore


uppercaseNameUsingRecursiveAndThenUsingEmptiableWithoutReverseHelper : Core.Parser ModuleName
uppercaseNameUsingRecursiveAndThenUsingEmptiableWithoutReverseHelper =
    Core.oneOf
        [ Core.succeed identity
            |. Tokens.dot
            |= Tokens.typeName
            |> Core.andThen (\t -> Core.map (\further -> t :: further) uppercaseNameUsingRecursiveAndThenUsingEmptiableWithoutReverseHelper)
        , Core.succeed []
        ]


uppercaseNameUsingSep1 : Core.Parser (Node ModuleName)
uppercaseNameUsingSep1 =
    sepBy1 "." Tokens.typeName
        |> Node.parserCore


sepBy1 : String -> Core.Parser a -> Core.Parser (List a)
sepBy1 sep p =
    Core.succeed listCons
        |= p
        |= many
            (Core.succeed identity
                |. Core.symbol sep
                |= p
            )


many : Core.Parser a -> Core.Parser (List a)
many p =
    manyWithoutReverse [] p
        |> Core.map List.reverse


manyWithoutReverse : List a -> Core.Parser a -> Core.Parser (List a)
manyWithoutReverse initList p =
    let
        helper : List a -> Core.Parser (Core.Step (List a) (List a))
        helper items =
            Core.oneOf
                [ p
                    |> Core.map (\item -> Core.Loop (item :: items))
                , Core.succeed (Core.Done items)
                ]
    in
    Core.loop initList helper


uppercaseNameUsingLoopUsingStringAndFinalSplit : Core.Parser (Node ModuleName)
uppercaseNameUsingLoopUsingStringAndFinalSplit =
    Tokens.typeName
        |> Core.andThen
            (\typeOrSegment ->
                Core.loop typeOrSegment
                    uppercaseNameUsingLoopUsingStringHelper
            )
        |> Node.parserCore


uppercaseNameUsingLoopUsingStringHelper : String -> Core.Parser (Core.Step String (List String))
uppercaseNameUsingLoopUsingStringHelper moduleNameSoFar =
    Core.oneOf
        [ Core.succeed (\t -> Core.Loop (moduleNameSoFar ++ "." ++ t))
            |. Tokens.dot
            |= Tokens.typeName
        , Core.lazy (\() -> Core.succeed (Core.Done (moduleNameSoFar |> String.split ".")))
        ]


uppercaseNameUsingLoopUsingEmptiable : Core.Parser (Node ModuleName)
uppercaseNameUsingLoopUsingEmptiable =
    Tokens.typeName
        |> Core.andThen
            (\typeOrSegment ->
                Core.loop (typeOrSegment |> List.singleton)
                    uppercaseNameUsingLoopUsingEmptiableHelper
            )
        |> Node.parserCore


uppercaseNameUsingLoopUsingEmptiableHelper : ModuleName -> Core.Parser (Core.Step ModuleName ModuleName)
uppercaseNameUsingLoopUsingEmptiableHelper moduleNameSoFar =
    Core.oneOf
        [ Core.succeed (\t -> Core.Loop (t :: moduleNameSoFar))
            |. Tokens.dot
            |= Tokens.typeName
        , Core.lazy (\() -> Core.succeed (Core.Done (List.reverse moduleNameSoFar)))
        ]


uppercaseNameUsingRecursiveAndThenUsingEmptiable : Core.Parser (Node ModuleName)
uppercaseNameUsingRecursiveAndThenUsingEmptiable =
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> uppercaseNameUsingRecursiveAndThenUsingEmptiableHelper (typeOrSegment |> List.singleton))
        |> Node.parserCore


uppercaseNameUsingRecursiveAndThenUsingEmptiableHelper : ModuleName -> Core.Parser ModuleName
uppercaseNameUsingRecursiveAndThenUsingEmptiableHelper moduleNameSoFar =
    Core.oneOf
        [ Core.succeed identity
            |. Tokens.dot
            |= Tokens.typeName
            |> Core.andThen (\t -> uppercaseNameUsingRecursiveAndThenUsingEmptiableHelper (t :: moduleNameSoFar))
        , Core.lazy (\() -> Core.succeed (List.reverse moduleNameSoFar))
        ]


uppercaseNameUsingRecursiveAndThen : Core.Parser (Node ModuleName)
uppercaseNameUsingRecursiveAndThen =
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> uppercaseNameUsingRecursiveAndThenHelper [] typeOrSegment)
        |> Node.parserCore


uppercaseNameUsingRecursiveAndThenHelper : ModuleName -> String -> Core.Parser ModuleName
uppercaseNameUsingRecursiveAndThenHelper moduleNameSoFar typeOrSegment =
    Core.oneOf
        [ Core.succeed identity
            |. Tokens.dot
            |= Tokens.typeName
            |> Core.andThen (\t -> uppercaseNameUsingRecursiveAndThenHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed (List.reverse (typeOrSegment :: moduleNameSoFar)))
        ]


exampleEscapedCharValueList : List String
exampleEscapedCharValueList =
    [ "'", "\"", "\\", "\u{000D}", "\t", "t", "n", "r", "\n" ] ++ exampleListOfStrings


escapedCharValueUsingMapLazy : Core.Parser Char
escapedCharValueUsingMapLazy =
    Core.oneOf
        [ Core.map (\() -> '\'') (Core.symbol "'")
        , Core.map (\() -> '"') (Core.symbol "\"")
        , Core.map (\() -> '\n') (Core.symbol "n")
        , Core.map (\() -> '\t') (Core.symbol "t")
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Core.map (\() -> '\u{000D}') (Core.symbol "r")
        , Core.map (\() -> '\\') (Core.symbol "\\")
        , Core.succeed
            (\hex ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            |. Core.symbol "u{"
            |= (Core.chompWhile Char.isHexDigit |> Core.getChompedString)
            |. Core.symbol "}"
        ]


escapedCharValueUsingSucceedIgnore : Core.Parser Char
escapedCharValueUsingSucceedIgnore =
    Core.oneOf
        [ Core.succeed '\'' |. Core.symbol "'"
        , Core.succeed '"' |. Core.symbol "\""
        , Core.succeed '\n' |. Core.symbol "n"
        , Core.succeed '\t' |. Core.symbol "t"
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Core.succeed '\u{000D}' |. Core.symbol "r"
        , Core.succeed '\\' |. Core.symbol "\\"
        , Core.succeed
            (\hex ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            |. Core.symbol "u{"
            |= (Core.chompWhile Char.isHexDigit |> Core.getChompedString)
            |. Core.symbol "}"
        ]


minusSymbolsUsingSharedSymbolInEachBranch : Core.Parser ()
minusSymbolsUsingSharedSymbolInEachBranch =
    Core.oneOf
        [ Core.symbol "- "
        , Core.symbol "-\n"
        , Core.symbol "-\u{000D}"
        ]


minusSymbolsUsingSharedSymbolThenEachBranch : Core.Parser ()
minusSymbolsUsingSharedSymbolThenEachBranch =
    Core.symbol "-"
        |. Core.oneOf
            [ Core.symbol " "
            , Core.symbol "\n"
            , Core.symbol "\u{000D}"
            ]


exampleMinusSymbols : List String
exampleMinusSymbols =
    [ "- "
    , "-\n"
    , "+"
    , "--"
    , "-\u{000D}"
    , "---"
    , "-("
    , " -"
    , "x"
    , " "
    , "\n"
    , "-"
    , "- "
    , "-\n"
    , "-\u{000D}"
    ]


reservedList : Set String
reservedList =
    [ "module"
    , "exposing"
    , "import"
    , "as"
    , "if"
    , "then"
    , "else"
    , "let"
    , "in"
    , "case"
    , "of"
    , "port"

    --, "infixr"
    --, "infixl"
    , "type"

    --, "infix" Apparently this is not a reserved keyword
    --, "alias" Apparently this is not a reserved keyword
    , "where"
    ]
        |> Set.fromList


functionNameUsingReservedWordSet : Core.Parser String
functionNameUsingReservedWordSet =
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = reservedList
        }


filterNonReservedUsingCase : String -> Core.Parser String
filterNonReservedUsingCase name =
    case name of
        "module" ->
            Core.problem "reversed word"

        "exposing" ->
            Core.problem "reversed word"

        "import" ->
            Core.problem "reversed word"

        "as" ->
            Core.problem "reversed word"

        "if" ->
            Core.problem "reversed word"

        "then" ->
            Core.problem "reversed word"

        "else" ->
            Core.problem "reversed word"

        "let" ->
            Core.problem "reversed word"

        "in" ->
            Core.problem "reversed word"

        "case" ->
            Core.problem "reversed word"

        "of" ->
            Core.problem "reversed word"

        "port" ->
            Core.problem "reversed word"

        "type" ->
            Core.problem "reversed word"

        "where" ->
            Core.problem "reversed word"

        nonReserved ->
            Core.commit nonReserved


isReservedUsingCase : String -> Bool
isReservedUsingCase name =
    case name of
        "module" ->
            True

        "exposing" ->
            True

        "import" ->
            True

        "as" ->
            True

        "if" ->
            True

        "then" ->
            True

        "else" ->
            True

        "let" ->
            True

        "in" ->
            True

        "case" ->
            True

        "of" ->
            True

        "port" ->
            True

        "type" ->
            True

        "where" ->
            True

        _ ->
            False


functionNameUsingReservedWordCase : Core.Parser String
functionNameUsingReservedWordCase =
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }
        |> Core.backtrackable
        |> Core.andThen filterNonReservedUsingCase


functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcut : Core.Parser String
functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcut =
    Core.variable
        { start = Unicode.isLower
        , inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.empty
        }
        |> Core.backtrackable
        |> Core.andThen filterNonReservedUsingCase


functionNameUsingReservedWordOneOfAndCharIsAlphaNumShortcut : Parser String
functionNameUsingReservedWordOneOfAndCharIsAlphaNumShortcut =
    Core.oneOf
        [ Core.symbol "module" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "exposing" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "import" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "as" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "if" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "then" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "else" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "let" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "in" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "case" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "of" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "port" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "type" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.symbol "where" |> Core.andThen (\() -> Core.problem "reversed word")
        , Core.variable
            { start = Unicode.isLower
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.empty
            }
        ]


aFunctionName : Parser String
aFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "s"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'a' nameAfterFirstChar)
        , Core.succeed "a"
        ]


cFunctionName : Parser String
cFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "ase"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'c' nameAfterFirstChar)
        , Core.succeed "c"
        ]


eFunctionName : Parser String
eFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "xposing" |> Set.insert "lse"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'e' nameAfterFirstChar)
        , Core.succeed "e"
        ]


iFunctionName : Parser String
iFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "f" |> Set.insert "mport" |> Set.insert "n"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'i' nameAfterFirstChar)
        , Core.succeed "i"
        ]


lFunctionName : Parser String
lFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "et"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'l' nameAfterFirstChar)
        , Core.succeed "l"
        ]


mFunctionName : Parser String
mFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "odule"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'm' nameAfterFirstChar)
        , Core.succeed "m"
        ]


oFunctionName : Parser String
oFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "f"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'o' nameAfterFirstChar)
        , Core.succeed "o"
        ]


pFunctionName : Parser String
pFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "ort"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'p' nameAfterFirstChar)
        , Core.succeed "p"
        ]


tFunctionName : Parser String
tFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "ype" |> Set.insert "hen"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 't' nameAfterFirstChar)
        , Core.succeed "t"
        ]


wFunctionName : Parser String
wFunctionName =
    Core.oneOf
        [ Core.variable
            { start =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , inner =
                \c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            , reserved = Set.singleton "here"
            }
            |> Core.map (\nameAfterFirstChar -> String.cons 'w' nameAfterFirstChar)
        , Core.succeed "w"
        ]


functionNameAfterFirst : Parser String
functionNameAfterFirst =
    Core.variable
        { start =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.empty
        }


functionNameUsingReservedWordOneOfBasedOnFirstCharAndCharIsAlphaNumShortcut : Parser String
functionNameUsingReservedWordOneOfBasedOnFirstCharAndCharIsAlphaNumShortcut =
    Core.chompIf (\_ -> True)
        |> Core.getChompedString
        |> Core.andThen
            (\firstChar ->
                -- check for a first char possible on a reserved word
                case firstChar of
                    "a" ->
                        aFunctionName

                    "c" ->
                        cFunctionName

                    "e" ->
                        eFunctionName

                    "i" ->
                        iFunctionName

                    "l" ->
                        lFunctionName

                    "m" ->
                        mFunctionName

                    "o" ->
                        oFunctionName

                    "p" ->
                        pFunctionName

                    "t" ->
                        tFunctionName

                    "w" ->
                        wFunctionName

                    otherFirstChar ->
                        if String.all Unicode.isLower otherFirstChar then
                            functionNameAfterFirst
                                |> Core.map (\restName -> otherFirstChar ++ restName)

                        else
                            Core.problem "invalid first variable name character"
            )


functionNameUsingReservedWordCaseCheckAndCharIsAlphaNumShortcut : Core.Parser String
functionNameUsingReservedWordCaseCheckAndCharIsAlphaNumShortcut =
    Core.variable
        { start = Unicode.isLower
        , inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.empty
        }
        |> Core.backtrackable
        |> Core.andThen
            (\name ->
                if isReservedUsingCase name then
                    Core.problem "reserved word"

                else
                    Core.commit name
            )


functionNameUsingReservedWordComparisonsAndCharIsAlphaNumShortcut : Core.Parser String
functionNameUsingReservedWordComparisonsAndCharIsAlphaNumShortcut =
    Core.variable
        { start = Unicode.isLower
        , inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.empty
        }
        |> Core.backtrackable
        |> Core.andThen
            (\name ->
                if isReservedUsingComparisons name then
                    Core.problem "reserved word"

                else
                    Core.commit name
            )


functionNameUsingReservedWordSetAndCharIsAlphaNumShortcut : Core.Parser String
functionNameUsingReservedWordSetAndCharIsAlphaNumShortcut =
    Core.variable
        { start = Unicode.isLower
        , inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = reservedList
        }


functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcutAndCustomLogic : Core.Parser String
functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcutAndCustomLogic =
    Core.chompIf Unicode.isLower
        |. Core.chompWhile
            (\c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
            )
        |> Core.getChompedString
        |> Core.backtrackable
        |> Core.andThen filterNonReservedUsingCase


functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcutAndLogicWithVariableOnlyForFirst : Core.Parser String
functionNameUsingReservedWordCaseAndCharIsAlphaNumShortcutAndLogicWithVariableOnlyForFirst =
    Core.succeed (++)
        |= Core.variable
            { start = Unicode.isLower
            , inner = \_ -> False
            , reserved = reservedList
            }
        |= (Core.chompWhile
                (\c ->
                    -- checking for Char.isAlphaNum early is much faster
                    Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
                )
                |> Core.getChompedString
           )
        |> Core.backtrackable
        |> Core.andThen filterNonReservedUsingCase


isReservedUsingComparisons : String -> Bool
isReservedUsingComparisons name =
    (name == "module")
        || (name == "exposing")
        || (name == "import")
        || (name == "as")
        || (name == "if")
        || (name == "then")
        || (name == "else")
        || (name == "let")
        || (name == "in")
        || (name == "case")
        || (name == "of")
        || (name == "port")
        || (name == "type")
        || (name == "where")


functionNameUsingReservedWordComparisons : Core.Parser String
functionNameUsingReservedWordComparisons =
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }
        |> Core.andThen
            (\name ->
                if isReservedUsingComparisons name then
                    Core.problem "reversed word"

                else
                    Core.succeed name
            )


listReverseStringConcatUsingListReverseStringConcatUsing : List String -> String
listReverseStringConcatUsingListReverseStringConcatUsing list =
    list |> List.reverse |> String.concat


listReverseStringConcatUsingRecursive : List String -> String
listReverseStringConcatUsingRecursive list =
    listReverseStringConcatUsingRecursiveOnto "" list


listReverseStringConcatUsingRecursiveOnto : String -> List String -> String
listReverseStringConcatUsingRecursiveOnto soFar list =
    case list of
        [] ->
            soFar

        head :: tail ->
            listReverseStringConcatUsingRecursiveOnto (head ++ soFar) tail


stringLiteralUsingConsReverse : Parser String
stringLiteralUsingConsReverse =
    Core.succeed identity
        |. Core.symbol "\""
        |= Core.loop [] stringLiteralUsingConsReverseHelper


stringLiteralUsingConsReverseHelper : List String -> Parser (Step (List String) String)
stringLiteralUsingConsReverseHelper partsSoFarReverse =
    Core.oneOf
        [ Core.symbol "\"" |> Core.map (\() -> Done (partsSoFarReverse |> listReverseStringConcatUsingRecursive))
        , Core.succeed (\v -> Loop (String.fromChar v :: partsSoFarReverse))
            |. Core.symbol "\\"
            |= escapedCharValue
        , Core.mapChompedString
            (\value () -> Loop (value :: partsSoFarReverse))
            (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
        ]


stringLiteralUsingLoopChompThenGetChompedStep :
    List { offset : Int, sourceLength : Int, unescaped : Char }
    -> Parser (Step (List { offset : Int, sourceLength : Int, unescaped : Char }) (List { offset : Int, sourceLength : Int, unescaped : Char }))
stringLiteralUsingLoopChompThenGetChompedStep escaped =
    Core.oneOf
        [ Core.symbol "\"" |> Core.map (\() -> Done escaped)
        , Core.succeed (\v -> Loop escaped)
            -- TODO escaped
            |. Core.symbol "\\"
            |= escapedCharValue
        , Core.map
            (\() -> Loop escaped)
            (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
        ]


stringLiteralUsingAppendAfter : Parser String
stringLiteralUsingAppendAfter =
    Core.succeed identity
        |. Core.symbol "\""
        |= Core.loop "" stringLiteralUsingAppendAfterHelper


stringLiteralUsingAppendAfterHelper : String -> Parser (Step String String)
stringLiteralUsingAppendAfterHelper stringSoFar =
    Core.oneOf
        [ Core.symbol "\"" |> Core.map (\() -> Done stringSoFar)
        , Core.succeed (\v -> Loop (stringSoFar ++ String.fromChar v))
            |. Core.symbol "\\"
            |= escapedCharValue
        , Core.mapChompedString
            (\value () -> Loop (stringSoFar ++ value))
            (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
        ]


escapedCharValue : Core.Parser Char
escapedCharValue =
    Core.oneOf
        [ Core.succeed '\'' |. Core.symbol "'"
        , Core.succeed '"' |. Core.symbol "\""
        , Core.succeed '\n' |. Core.symbol "n"
        , Core.succeed '\t' |. Core.symbol "t"
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Core.succeed '\u{000D}' |. Core.symbol "r"
        , Core.succeed '\\' |. Core.symbol "\\"
        , Core.succeed
            (\hex ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            |. Core.symbol "u"
            |. Core.symbol "{"
            |= (Core.chompWhile Char.isHexDigit |> Core.getChompedString)
            |. Core.symbol "}"
        ]


exampleListOfStrings : List String
exampleListOfStrings =
    List.repeat 10
        [ "You can remove these two functions."
        , ">>"
        , "A part of this condition is unnecessary. \u{000D} You can remove it and it would not impact the behavior of the program."
        , "This condition will always result in the same value. ' You may have hardcoded a value or mistyped a condition '."
        , "Part of the expression is unnecessary"
        , "Based on the values and/or the context, we can determine that the value of this operation will always be "
        , "."
        , "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own."
        , "`identity` should be removed"
        , "\" or \"The \t way"
        ]
        |> List.concat


exampleListOfVariableNames : List String
exampleListOfVariableNames =
    (List.range 0 1
        |> List.concatMap
            (\i ->
                [ "You can remove these two functions."
                , "describe"
                , "map"
                , "subscriptions"
                , "main"
                , "letter"
                , "caseByCase"
                , "often"
                , "repeat"
                , "concatMap"
                , "sequence"
                , "traverse"
                , "andThen"
                , "update"
                , "view"
                , "button"
                , "slider"
                , "parseXml"
                , "getAtIndex"
                , "createFrom"
                , "singleton"
                , "map2"
                , "merge"
                , "foldFrom2"
                , "wait_for_it"
                , "untagged"
                , "toMaybe"
                , "recover"
                , "mapError"
                , "sleep"
                , "stabilize"
                , "rotate"
                , "skew"
                , "thisIsLikelyLongerThanMostVariableNameIn_Practice"
                , "Part of the expression is unnecessary"
                ]
                    |> List.map (\name -> name ++ String.fromInt i)
            )
    )
        ++ [ ">>"
           , "."
           , "This condition will always result in the same value. ' You may have hardcoded a value or mistyped a condition '."
           ]
        ++ (reservedList |> Set.toList)
