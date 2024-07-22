module Elm.Parser.Node exposing (parser, parserCore, parserFromCore)

import Combine exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|=))


parser : Parser state a -> Parser state (Node a)
parser p =
    Combine.succeed
        (\( startRow, startColumn ) ->
            \v ->
                \( endRow, endColumn ) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
                        v
        )
        |> Combine.keepFromCore Core.getPosition
        |> Combine.keep p
        |> Combine.keepFromCore Core.getPosition


parserFromCore : Core.Parser a -> Parser state (Node a)
parserFromCore p =
    parserCore p
        |> Combine.fromCore


parserCore : Core.Parser a -> Core.Parser (Node a)
parserCore p =
    Core.succeed
        (\( startRow, startColumn ) ->
            \v ->
                \( endRow, endColumn ) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
                        v
        )
        |= Core.getPosition
        |= p
        |= Core.getPosition
