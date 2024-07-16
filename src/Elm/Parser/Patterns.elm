module Elm.Parser.Patterns exposing (pattern)

import Combine exposing (Parser)
import Elm.Parser.Base as Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Parser as Core exposing ((|.))


tryToCompose : Node Pattern -> Parser State (Node Pattern)
tryToCompose x =
    Combine.maybeIgnore Layout.layout
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.succeed (\y -> Node.combine AsPattern x y)
                    |> Combine.ignoreEntirely (Core.keyword "as")
                    |> Combine.ignore Layout.layout
                    |> Combine.keep (Node.parserFromCore Tokens.functionName)
                , Combine.succeed (\y -> Node.combine UnConsPattern x y)
                    |> Combine.ignoreEntirely (Core.symbol "::")
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
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
        (Combine.parens (Combine.sepBy "," (Layout.maybeAroundBothSides pattern))
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
    Node.parserCore (Core.map VarPattern Tokens.functionName)
        |> Combine.fromCore


numberPart : Parser state (Node Pattern)
numberPart =
    Elm.Parser.Numbers.number IntPattern HexPattern
        |> Node.parserFromCore


charPattern : Parser state (Node Pattern)
charPattern =
    Tokens.characterLiteral
        |> Core.map CharPattern
        |> Node.parserCore
        |> Combine.fromCore


listPattern : Parser State (Node Pattern)
listPattern =
    Node.parser <|
        Combine.between
            (Combine.symbol "[" |> Combine.ignore (Combine.maybeIgnore Layout.layout))
            (Combine.symbol "]")
            (Combine.map ListPattern (Combine.sepBy "," (Layout.maybeAroundBothSides pattern)))


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
    Tokens.stringLiteral
        |> Core.map StringPattern
        |> Node.parserCore
        |> Combine.fromCore


qualifiedPattern : ConsumeArgs -> Parser State (Node Pattern)
qualifiedPattern consumeArgs =
    Base.typeIndicator
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\(Node range ( mod, name )) ->
                (if consumeArgs then
                    Combine.manyWithEndLocationForLastElement range Node.range (qualifiedPatternArg |> Combine.ignore (Combine.maybeIgnore Layout.layout))

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
            Combine.between
                (Combine.symbol "{" |> Combine.continueWith (Combine.maybeIgnore Layout.layout))
                (Combine.symbol "}")
                (Combine.sepBy "," (Layout.maybeAroundBothSides (Node.parserFromCore Tokens.functionName)))
        )
