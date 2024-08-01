module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import Parser exposing ((|.), Nestable(..), Parser)
import Parser.Extra


singleLineCommentCore : Parser.Parser String
singleLineCommentCore =
    Parser.symbol "--"
        |. Parser.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')
        |> Parser.getChompedString


multilineCommentString : Parser.Parser String
multilineCommentString =
    Parser.oneOf
        [ Parser.symbol "{-|"
            |> Parser.Extra.continueWith (Parser.problem "unexpected documentation comment")
        , Parser.multiComment "{-" "-}" Nestable
            |> Parser.getChompedString
        ]
        |> Parser.backtrackable


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : Parser.Parser (Node Documentation)
declarationDocumentation =
    Parser.oneOf
        [ -- if the next symbol isn't "{-|", we commit to failure
          (Parser.symbol "{-" |. Parser.chompIf (\c -> c /= '|'))
            |> Parser.backtrackable
            |> Parser.Extra.continueWith (Parser.problem "multiline comment should be documentation comment")
        , Parser.multiComment "{-" "-}" Nestable
            |> Parser.getChompedString
            |> Node.parserCore
        ]
        |> Parser.backtrackable
