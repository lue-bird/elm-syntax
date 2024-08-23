module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import ParserFast exposing (Parser)


singleLineCommentCore : ParserFast.Parser String
singleLineCommentCore =
    ParserFast.symbol2FollowedBy '-'
        '-'
        (ParserFast.whileMap
            (\c -> c /= '\u{000D}' && c /= '\n')
            (\content -> "--" ++ content)
        )


multilineCommentString : ParserFast.Parser String
multilineCommentString =
    ParserFast.andThenWithRemaining
        (\remaining ->
            case remaining of
                '{' :: '-' :: '|' :: _ ->
                    problemUnexpectedDocumentation

                _ ->
                    multiLineCommentStringNoCheck
        )


problemUnexpectedDocumentation : Parser a
problemUnexpectedDocumentation =
    ParserFast.problem "unexpected documentation comment"


multiLineCommentStringNoCheck : Parser String
multiLineCommentStringNoCheck =
    ParserFast.nestableMultiComment ( '{', "-" ) ( '-', "}" )


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : ParserFast.Parser (Node Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    ParserFast.nestableMultiComment ( '{', "-" ) ( '-', "}" )
        |> Node.parserCore
