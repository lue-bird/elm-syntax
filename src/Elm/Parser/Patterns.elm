module Elm.Parser.Patterns exposing (pattern)

import Combine exposing (Parser, between, lazy, many, maybe, parens, sepBy, string)
import Elm.Parser.Base as Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (characterLiteral, functionName, stringLiteral)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range as Range exposing (Range)
import Parser as Core


tryToCompose : Node Range (Pattern Range) -> Parser State (Node Range (Pattern Range))
tryToCompose x =
    maybe Layout.layout
        |> Combine.continueWith
            (Combine.choice
                [ Combine.fromCore (Core.keyword "as")
                    |> Combine.ignore Layout.layout
                    |> Combine.continueWith (Node.parser functionName)
                    |> Combine.map (\y -> Node.combine AsPattern x y)
                , Combine.fromCore (Core.symbol "::")
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith pattern
                    |> Combine.map (\y -> Node.combine UnConsPattern x y)
                , Combine.succeed x
                ]
            )


pattern : Parser State (Node Range (Pattern Range))
pattern =
    composablePattern |> Combine.andThen tryToCompose


parensPattern : Parser State (Node Range (Pattern Range))
parensPattern =
    Combine.lazy
        (\() ->
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
        )


variablePart : Parser State (Node Range (Pattern Range))
variablePart =
    Node.parser (Combine.map VarPattern functionName)


numberPart : Parser State (Pattern Range)
numberPart =
    Elm.Parser.Numbers.integer IntPattern HexPattern


listPattern : Parser State (Node Range (Pattern Range))
listPattern =
    lazy
        (\() ->
            Node.parser <|
                between
                    (string "[" |> Combine.ignore (maybe Layout.layout))
                    (string "]")
                    (Combine.map ListPattern (sepBy (string ",") (Layout.maybeAroundBothSides pattern)))
        )


type alias ConsumeArgs =
    Bool


composablePattern : Parser State (Node Range (Pattern Range))
composablePattern =
    Combine.choice
        [ variablePart
        , qualifiedPattern True
        , Node.parser (stringLiteral |> Combine.map StringPattern)
        , Node.parser (characterLiteral |> Combine.map CharPattern)
        , Node.parser numberPart
        , Node.parser (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern))
        , Node.parser (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern))
        , recordPattern
        , listPattern
        , parensPattern
        ]


qualifiedPatternArg : Parser State (Node Range (Pattern Range))
qualifiedPatternArg =
    Combine.choice
        [ variablePart
        , qualifiedPattern False
        , Node.parser (stringLiteral |> Combine.map StringPattern)
        , Node.parser (characterLiteral |> Combine.map CharPattern)
        , Node.parser numberPart
        , Node.parser (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern))
        , Node.parser (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern))
        , recordPattern
        , listPattern
        , parensPattern
        ]


qualifiedPattern : ConsumeArgs -> Parser State (Node Range (Pattern Range))
qualifiedPattern consumeArgs =
    Node.parser Base.typeIndicator
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\(Node range ( mod, name )) ->
                (if consumeArgs then
                    many (qualifiedPatternArg |> Combine.ignore (maybe Layout.layout))

                 else
                    Combine.succeed []
                )
                    |> Combine.map
                        (\args ->
                            Node
                                (Range.combine (range :: List.map (\(Node r _) -> r) args))
                                (NamedPattern (QualifiedNameRef mod name) args)
                        )
            )


recordPattern : Parser State (Node Range (Pattern Range))
recordPattern =
    lazy
        (\() ->
            Node.parser
                (Combine.map RecordPattern <|
                    between
                        (string "{" |> Combine.continueWith (maybe Layout.layout))
                        (string "}")
                        (sepBy (string ",") (Layout.maybeAroundBothSides (Node.parser functionName)))
                )
        )
