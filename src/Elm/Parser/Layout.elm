module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , moduleLevelIndentation
    , onTopIndentation
    , optimisticLayout
    , positivelyIndented
    , positivelyIndentedPlus
    )

import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (Comments, WithComments)
import Rope
import Set


whitespaceAndCommentsOrEmpty : Parser.Parser Comments
whitespaceAndCommentsOrEmpty =
    Parser.oneOf
        [ whitespace
            -- whitespace can't be followed by more whitespace
            |> Parser.andThen (\_ -> fromCommentElseEmpty)
        , fromCommentElseEmpty
        ]


whitespace : Parser String
whitespace =
    Parser.variable
        { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
        , reserved = Set.empty
        , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
        }


fromCommentElseEmpty : Parser Comments
fromCommentElseEmpty =
    -- since comments are comparatively rare
    -- but expensive to check for, we allow shortcutting to the end
    Parser.oneOf
        [ -- this one is tricky!
          -- if the next character is not a start of a comment,
          --     the chomp fails and we fully return with no comments.
          -- if the next character _is_ possibly the start of a comment, we commit to a temporaryProblem, making the inner oneOf fail
          --     The inner oneOf is backtrackable though, so we continue below, checking for comments or end
          Parser.oneOf
            [ Parser.chompIf (\c -> c == '-' || c == '{')
                |> Parser.Extra.continueWith temporaryProblem
            , Parser.succeed Rope.empty
            ]
            |> Parser.backtrackable
        , fromSingleLineCommentNode
        , fromMultilineCommentNode
        , Parser.succeed Rope.empty
        ]


temporaryProblem : Parser a
temporaryProblem =
    Parser.problem ""


fromMultilineCommentNode : Parser Comments
fromMultilineCommentNode =
    Node.parserCoreMap
        (\comment ->
            \commentsAfter ->
                Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        Comments.multilineCommentString
        |= Parser.lazy (\() -> whitespaceAndCommentsOrEmpty)


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    Parser.map
        (\comment ->
            \commentsAfter ->
                Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        Comments.singleLineCommentCore
        |= Parser.lazy (\() -> whitespaceAndCommentsOrEmpty)


maybeLayout : Parser Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty
        |. positivelyIndented


{-| Use to check that the indentation of an already parsed token
would be valid for [`positivelyIndented`](#positivelyIndented)
-}
positivelyIndentedPlus : Int -> Parser.Parser ()
positivelyIndentedPlus extraIndent =
    Parser.andThen
        (\column ->
            Parser.andThen
                (\indent ->
                    if column > indent + extraIndent then
                        succeedUnit

                    else
                        problemPositivelyIndented
                )
                Parser.getIndent
        )
        Parser.getCol


positivelyIndented : Parser.Parser ()
positivelyIndented =
    Parser.getCol
        |> Parser.andThen
            (\column ->
                Parser.andThen
                    (\indent ->
                        if column > indent then
                            succeedUnit

                        else
                            problemPositivelyIndented
                    )
                    Parser.getIndent
            )


succeedUnit : Parser ()
succeedUnit =
    Parser.succeed ()


problemPositivelyIndented : Parser a
problemPositivelyIndented =
    Parser.problem "must be positively indented"


layout : Parser Comments
layout =
    Parser.oneOf
        [ whitespace
            |> Parser.andThen (\_ -> fromCommentElseEmpty)
        , fromSingleLineCommentNode
        , fromMultilineCommentNode
        ]
        |. positivelyIndented


optimisticLayout : Parser Comments
optimisticLayout =
    whitespaceAndCommentsOrEmpty


layoutStrict : Parser Comments
layoutStrict =
    optimisticLayout
        |. onTopIndentation ()


moduleLevelIndentation : res -> Parser res
moduleLevelIndentation res =
    let
        succeedRes : Parser res
        succeedRes =
            Parser.succeed res
    in
    Parser.andThen
        (\column ->
            if column == 1 then
                succeedRes

            else
                problemModuleLevelIndentation
        )
        Parser.getCol


problemModuleLevelIndentation : Parser.Parser a
problemModuleLevelIndentation =
    Parser.problem "must be on module-level indentation"


onTopIndentation : res -> Parser res
onTopIndentation res =
    let
        succeedRes : Parser res
        succeedRes =
            Parser.succeed res
    in
    Parser.andThen
        (\column ->
            Parser.andThen
                (\indent ->
                    if column == indent then
                        succeedRes

                    else
                        problemTopIndentation
                )
                Parser.getIndent
        )
        Parser.getCol


problemTopIndentation : Parser.Parser a
problemTopIndentation =
    Parser.problem "must be on top indentation"


maybeAroundBothSides : Parser (WithComments b) -> Parser (WithComments b)
maybeAroundBothSides x =
    Parser.map
        (\before ->
            \v ->
                \after ->
                    { comments =
                        before
                            |> Rope.prependTo v.comments
                            |> Rope.prependTo after
                    , syntax = v.syntax
                    }
        )
        maybeLayout
        |= x
        |= maybeLayout
