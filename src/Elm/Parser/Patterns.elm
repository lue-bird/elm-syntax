module Elm.Parser.Patterns exposing (pattern)

import Combine exposing (Parser, between, maybe, parens, sepBy, string)
import Elm.Parser.Base as Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (characterLiteral, functionName, functionNameCore, stringLiteral)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Parser as Core exposing ((|.))


tryToCompose : Node Pattern -> Parser State (Node Pattern)
tryToCompose x =
    maybe Layout.layout
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.succeed (\y -> Node.combine AsPattern x y)
                    |> Combine.ignore (Combine.fromCore (Core.keyword "as"))
                    |> Combine.ignore Layout.layout
                    |> Combine.keep (Node.parser functionName)
                , Combine.succeed (\y -> Node.combine UnConsPattern x y)
                    |> Combine.ignore (Combine.fromCore (Core.symbol "::"))
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.keep pattern
                , Combine.succeed x
                ]
            )


pattern : Parser State (Node Pattern)
pattern =
    Combine.lazy
        (\() ->
            composablePattern |> Combine.andThen tryToCompose
        )


parensPattern : Parser State (Node Pattern)
parensPattern =
    Node.parser
        (parens (sepBy (string ",") (Layout.maybeAroundBothSides pattern))
            |> Combine.map
                (\c ->
                    case c of
                        [ x ] ->
                            ParenthesizedPattern x

                        _ ->
                            TuplePattern c
                )
        )


variablePart : Parser state (Node Pattern)
variablePart =
    Node.parserCore (Core.map VarPattern functionNameCore)
        |> Combine.fromCore


numberPart : Parser state (Node Pattern)
numberPart =
    Elm.Parser.Numbers.number IntPattern HexPattern
        |> Node.parser


charPattern : Parser state (Node Pattern)
charPattern =
    characterLiteral
        |> Core.map CharPattern
        |> Node.parserCore
        |> Combine.fromCore


listPattern : Parser State (Node Pattern)
listPattern =
    Node.parser <|
        between
            (string "[" |> Combine.ignore (maybe Layout.layout))
            (string "]")
            (Combine.map ListPattern (sepBy (string ",") (Layout.maybeAroundBothSides pattern)))


type alias ConsumeArgs =
    Bool


composablePattern : Parser State (Node Pattern)
composablePattern =
    Combine.oneOf
        [ variablePart
        , qualifiedPattern True
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        , stringPattern
        , listPattern
        , numberPart
        , charPattern
        ]


qualifiedPatternArg : Parser State (Node Pattern)
qualifiedPatternArg =
    Combine.oneOf
        [ variablePart
        , qualifiedPattern False
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        , stringPattern
        , listPattern
        , numberPart
        , charPattern
        ]


allPattern : Parser state (Node Pattern)
allPattern =
    Core.succeed AllPattern
        |. Core.symbol "_"
        |> Node.parserCore
        |> Combine.fromCore


unitPattern : Parser state (Node Pattern)
unitPattern =
    Core.succeed UnitPattern
        |. Core.symbol "()"
        |> Node.parserCore
        |> Combine.fromCore


stringPattern : Parser state (Node Pattern)
stringPattern =
    stringLiteral
        |> Core.map StringPattern
        |> Node.parserCore
        |> Combine.fromCore


qualifiedPattern : ConsumeArgs -> Parser State (Node Pattern)
qualifiedPattern consumeArgs =
    Base.typeIndicator
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\(Node range ( mod, name )) ->
                (if consumeArgs then
                    Combine.manyWithEndLocationForLastElement range Node.range (qualifiedPatternArg |> Combine.ignore (maybe Layout.layout))

                 else
                    Combine.succeed ( range.end, [] )
                )
                    |> Combine.map
                        (\( end, args ) ->
                            Node
                                { start = range.start, end = end }
                                (NamedPattern (QualifiedNameRef mod name) args)
                        )
            )


recordPattern : Parser State (Node Pattern)
recordPattern =
    Node.parser
        (Combine.map RecordPattern <|
            between
                (string "{" |> Combine.continueWith (maybe Layout.layout))
                (string "}")
                (sepBy (string ",") (Layout.maybeAroundBothSides (Node.parser functionName)))
        )
