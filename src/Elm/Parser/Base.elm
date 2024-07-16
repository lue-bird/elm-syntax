module Elm.Parser.Base exposing (moduleName, typeIndicator)

import Combine exposing (Parser)
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=))


moduleName : Parser state (Node ModuleName)
moduleName =
    Combine.sepBy1Core "." Tokens.typeName
        |> Node.parserFromCore


typeIndicator : Parser state (Node ( ModuleName, String ))
typeIndicator =
    let
        helper : ModuleName -> String -> Core.Parser ( ModuleName, String )
        helper moduleNameSoFar typeOrSegment =
            Core.oneOf
                [ Core.succeed identity
                    |. Core.symbol "."
                    |= Tokens.typeName
                    |> Core.andThen (\t -> helper (typeOrSegment :: moduleNameSoFar) t)
                , Core.succeed ()
                    |> Core.map (\() -> ( List.reverse moduleNameSoFar, typeOrSegment ))
                ]
    in
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> helper [] typeOrSegment)
        |> Node.parserFromCore
