module Elm.Parser.Base exposing (moduleName)

import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import ParserFast


moduleName : ParserFast.Parser (Node ModuleName)
moduleName =
    ParserFast.map2WithStartAndEndLocation
        (\start head tail end ->
            Node { start = start, end = end } (head :: tail)
        )
        Tokens.typeName
        moduleNameOrEmpty


moduleNameOrEmpty : ParserFast.Parser ModuleName
moduleNameOrEmpty =
    ParserFast.map2OrSucceed
        (\head tail -> head :: tail)
        (ParserFast.symbolFollowedBy "." Tokens.typeName)
        (ParserFast.lazy (\() -> moduleNameOrEmpty))
        []
