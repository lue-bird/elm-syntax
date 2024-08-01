module Elm.Parser.Layout exposing
    ( layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , maybeLayoutUntil
    , maybeLayoutUntilIgnored
    , maybeLayoutUntilMap
    , maybeLayoutUntilWithComments
    , moduleLevelIndentation
    , onTopIndentation
    , optimisticLayout
    , positivelyIndented
    , positivelyIndentedPlus
    )

import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Parser exposing ((|.), (|=), Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope
import Set


{-| since comments are comparatively rare but expensive to check for,
we allow shortcutting to a given `Parser ()`.

Use [`maybeLayoutUntil`](#maybeLayoutUntil) for other kinds of end parsers

-}
maybeLayoutUntilIgnored : Parser () -> Parser.Parser Comments
maybeLayoutUntilIgnored end =
    let
        fromSingleLineCommentUntilEnd : Parser Comments
        fromSingleLineCommentUntilEnd =
            Parser.map
                (\comment ->
                    \commentsAfter ->
                        Rope.one comment |> Rope.filledPrependTo commentsAfter
                )
                Comments.singleLineCommentCore
                |= Parser.lazy (\() -> maybeLayoutUntilIgnored end)

        fromMultilineCommentNodeUntilEnd : Parser Comments
        fromMultilineCommentNodeUntilEnd =
            Parser.oneOf
                [ Node.parserCoreMap
                    (\comment ->
                        \commentsAfter ->
                            Rope.one comment |> Rope.filledPrependTo commentsAfter
                    )
                    Comments.multilineCommentString
                    |= Parser.lazy (\() -> maybeLayoutUntilIgnored end)
                , endNoComments
                ]

        endNoComments : Parser Comments
        endNoComments =
            positivelyIndented Rope.empty |. end

        endOrFromCommentElseEmptyThenEnd : Parser Comments
        endOrFromCommentElseEmptyThenEnd =
            Parser.oneOf
                [ endNoComments |> Parser.backtrackable
                , fromSingleLineCommentUntilEnd
                , fromMultilineCommentNodeUntilEnd
                ]
    in
    Parser.oneOf
        [ whitespace
            |> Parser.andThen (\_ -> endOrFromCommentElseEmptyThenEnd)
        , endNoComments |> Parser.backtrackable
        , fromSingleLineCommentUntilEnd
        , fromMultilineCommentNodeUntilEnd
        ]


maybeLayoutUntil : Parser res -> Parser.Parser (WithComments res)
maybeLayoutUntil end =
    maybeLayoutUntilMap
        (\syntax -> { comments = Rope.empty, syntax = syntax })
        end


maybeLayoutUntilMap : (a -> WithComments b) -> Parser a -> Parser.Parser (WithComments b)
maybeLayoutUntilMap resToWithComments end =
    let
        fromSingleLineCommentUntilEnd : Parser (WithComments b)
        fromSingleLineCommentUntilEnd =
            Parser.map
                (\comment ->
                    \after ->
                        { comments =
                            Rope.one comment
                                |> Rope.filledPrependTo after.comments
                        , syntax = after.syntax
                        }
                )
                Comments.singleLineCommentCore
                |= Parser.lazy (\() -> maybeLayoutUntilMap resToWithComments end)

        fromMultilineCommentNodeUntilEnd : Parser (WithComments b)
        fromMultilineCommentNodeUntilEnd =
            Parser.oneOf
                [ Node.parserCoreMap
                    (\comment ->
                        \after ->
                            { comments = Rope.one comment |> Rope.filledPrependTo after.comments
                            , syntax = after.syntax
                            }
                    )
                    Comments.multilineCommentString
                    |= Parser.lazy (\() -> maybeLayoutUntilMap resToWithComments end)
                , endNoComments
                ]

        endNoComments : Parser (WithComments b)
        endNoComments =
            positivelyIndented resToWithComments
                |= end

        endOrFromCommentElseEmptyThenEnd : Parser (WithComments b)
        endOrFromCommentElseEmptyThenEnd =
            Parser.oneOf
                [ endNoComments |> Parser.backtrackable
                , fromSingleLineCommentUntilEnd
                , fromMultilineCommentNodeUntilEnd
                ]
    in
    Parser.oneOf
        [ whitespace
            |> Parser.andThen (\_ -> endOrFromCommentElseEmptyThenEnd)
        , endNoComments |> Parser.backtrackable
        , fromSingleLineCommentUntilEnd
        , fromMultilineCommentNodeUntilEnd
        ]


{-| since comments are comparatively rare but expensive to check for,
we allow shortcutting to a given end parser.

Do **not** use it if the end parser can `succeed` in a branch (like [`ParserWithComments.many`](ParserWithComments#many)).
In that case, use [`maybeLayout`](#maybeLayout) `|= yourParser` instead.

Use [`maybeLayoutUntilIgnored`](#maybeLayoutUntilIgnored) if your end is a `Parser ()`

-}
maybeLayoutUntilWithComments : Parser (WithComments res) -> Parser.Parser (WithComments res)
maybeLayoutUntilWithComments end =
    let
        fromSingleLineCommentUntilEnd : Parser (WithComments res)
        fromSingleLineCommentUntilEnd =
            Parser.map
                (\comment ->
                    \after ->
                        { comments = Rope.one comment |> Rope.filledPrependTo after.comments
                        , syntax = after.syntax
                        }
                )
                Comments.singleLineCommentCore
                |= Parser.lazy (\() -> maybeLayoutUntilWithComments end)

        fromMultilineCommentNodeUntilEnd : Parser (WithComments res)
        fromMultilineCommentNodeUntilEnd =
            Parser.oneOf
                [ Node.parserCoreMap
                    (\comment ->
                        \after ->
                            { comments = Rope.one comment |> Rope.filledPrependTo after.comments
                            , syntax = after.syntax
                            }
                    )
                    Comments.multilineCommentString
                    |= Parser.lazy (\() -> maybeLayoutUntilWithComments end)
                , endNoComments
                ]

        endNoComments : Parser (WithComments res)
        endNoComments =
            positivelyIndented identity
                |= end

        fromCommentElseEmptyThenEnd : Parser (WithComments res)
        fromCommentElseEmptyThenEnd =
            Parser.andThen
                (\source ->
                    Parser.andThen
                        (\offset ->
                            case source |> String.slice offset (offset + 2) of
                                "--" ->
                                    fromSingleLineCommentUntilEnd

                                "{-" ->
                                    fromMultilineCommentNodeUntilEnd

                                _ ->
                                    endNoComments
                        )
                        Parser.getOffset
                )
                Parser.getSource

        endOrFromCommentElseEmptyThenEnd : Parser (WithComments res)
        endOrFromCommentElseEmptyThenEnd =
            Parser.oneOf
                [ endNoComments |> Parser.backtrackable
                , fromCommentElseEmptyThenEnd
                ]
    in
    Parser.oneOf
        [ whitespace
            |> Parser.andThen (\_ -> endOrFromCommentElseEmptyThenEnd)
        , endNoComments |> Parser.backtrackable
        , fromCommentElseEmptyThenEnd
        ]


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
    -- but expensive to check for, we allow shortcutting to dead end
    Parser.andThen
        (\source ->
            Parser.andThen
                (\offset ->
                    case source |> String.slice offset (offset + 2) of
                        "--" ->
                            -- this will always succeed from here, so no need to fall back to Rope.empty
                            fromSingleLineCommentNode

                        "{-" ->
                            fromMultilineCommentNodeOrEmptyOnProblem

                        _ ->
                            succeedRopeEmpty
                )
                Parser.getOffset
        )
        Parser.getSource


succeedRopeEmpty : Parser (Rope.Rope a)
succeedRopeEmpty =
    Parser.succeed Rope.empty


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    Parser.oneOf [ fromMultilineCommentNode, Parser.succeed Rope.empty ]


fromMultilineCommentNode : Parser Comments
fromMultilineCommentNode =
    Node.parserCoreMap
        (\comment ->
            \commentsAfter ->
                Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        Comments.multilineCommentString
        |= whitespaceAndCommentsOrEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    Parser.map
        (\comment ->
            \commentsAfter ->
                Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        Comments.singleLineCommentCore
        |= whitespaceAndCommentsOrEmpty


maybeLayout : Parser Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty
        |. positivelyIndented ()


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


positivelyIndented : res -> Parser.Parser res
positivelyIndented res =
    let
        succeedRes : Parser res
        succeedRes =
            Parser.succeed res
    in
    Parser.getCol
        |> Parser.andThen
            (\column ->
                Parser.andThen
                    (\indent ->
                        if column > indent then
                            succeedRes

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
        (\v ->
            \after ->
                { comments =
                    v.comments
                        |> Rope.prependTo after
                , syntax = v.syntax
                }
        )
        (maybeLayoutUntilWithComments x)
        |= maybeLayout
