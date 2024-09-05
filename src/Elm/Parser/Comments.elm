module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

import Char.Extra
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)


singleLineComment : ParserFast.Parser (Node String)
singleLineComment =
    ParserFast.symbolFollowedBy ParserFast.ExpectingSymbolMinusMinus
        "--"
        (ParserFast.whileMap
            (\c -> c /= '\u{000D}' && c /= '\n' && not (Char.Extra.isUtf16Surrogate c))
            (\content -> "--" ++ content)
        )
        |> ParserFast.mapWithRange Node


multilineComment : ParserFast.Parser (Node String)
multilineComment =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice (offset + 2) (offset + 3) source of
                "|" ->
                    problemUnexpectedDocumentation

                _ ->
                    multiLineCommentNoCheck
        )


problemUnexpectedDocumentation : Parser a
problemUnexpectedDocumentation =
    ParserFast.problem ParserFast.ExpectingMultilineCommentNotDocumentation


multiLineCommentNoCheck : Parser (Node String)
multiLineCommentNoCheck =
    ParserFast.nestableMultiCommentMapWithRange Node
        ParserFast.ExpectingSymbolCurlyOpenMinus
        ( '{', "-" )
        ParserFast.ExpectingSymbolMinusCurlyClose
        ( '-', "}" )


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : ParserFast.Parser (Node Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    multiLineCommentNoCheck
