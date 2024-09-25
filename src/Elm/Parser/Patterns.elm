module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


pattern : Parser (WithComments (Node Pattern))
pattern =
    ParserFast.map2
        (\leftMaybeConsed maybeAsExtension ->
            ( leftMaybeConsed
                |> Tuple.first
                |> Rope.prependTo (maybeAsExtension |> Tuple.first)
            , case maybeAsExtension |> Tuple.second of
                Nothing ->
                    leftMaybeConsed |> Tuple.second

                Just anotherName ->
                    Node.combine Pattern.AsPattern (leftMaybeConsed |> Tuple.second) anotherName
            )
        )
        (ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
            (ParserFast.map2
                (\startPatternResult commentsAfter ->
                    ( startPatternResult |> Tuple.first |> Rope.prependTo commentsAfter
                    , startPatternResult |> Tuple.second
                    )
                )
                (ParserFast.lazy (\() -> composablePattern))
                Layout.maybeLayout
            )
            (ParserFast.symbolFollowedBy "::"
                (ParserFast.map3
                    (\commentsAfterCons patternResult commentsAfterTailSubPattern ->
                        ( commentsAfterCons
                            |> Rope.prependTo (patternResult |> Tuple.first)
                            |> Rope.prependTo commentsAfterTailSubPattern
                        , patternResult |> Tuple.second
                        )
                    )
                    Layout.maybeLayout
                    (ParserFast.lazy (\() -> composablePattern))
                    Layout.maybeLayout
                )
            )
            (\consed afterCons ->
                ( consed |> Tuple.first |> Rope.prependTo (afterCons |> Tuple.first)
                , Node.combine Pattern.UnConsPattern (consed |> Tuple.second) (afterCons |> Tuple.second)
                )
            )
        )
        (ParserFast.orSucceed
            (ParserFast.keywordFollowedBy "as"
                (ParserFast.map2
                    (\commentsAfterAs name ->
                        ( commentsAfterAs
                        , Just name
                        )
                    )
                    Layout.maybeLayout
                    Tokens.functionNameNode
                )
            )
            ( Rope.empty, Nothing )
        )


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                ( commentsBeforeHead
                    |> Rope.prependTo (contentResult |> Tuple.first)
                , Node { start = { row = range.start.row, column = range.start.column - 1 }, end = range.end }
                    (contentResult |> Tuple.second)
                )
            )
            Layout.maybeLayout
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")" ( Rope.empty, UnitPattern ))
                (ParserFast.map3
                    (\headResult commentsAfterHead tailResult ->
                        ( headResult
                            |> Tuple.first
                            |> Rope.prependTo commentsAfterHead
                            |> Rope.prependTo (tailResult |> Tuple.first)
                        , case tailResult |> Tuple.second of
                            Nothing ->
                                ParenthesizedPattern (headResult |> Tuple.second)

                            Just secondAndMaybeThirdPart ->
                                case secondAndMaybeThirdPart.maybeThirdPart of
                                    Nothing ->
                                        TuplePattern [ headResult |> Tuple.second, secondAndMaybeThirdPart.secondPart ]

                                    Just thirdPart ->
                                        TuplePattern [ headResult |> Tuple.second, secondAndMaybeThirdPart.secondPart, thirdPart ]
                        )
                    )
                    pattern
                    Layout.maybeLayout
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" ( Rope.empty, Nothing ))
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map4
                                (\commentsBefore secondPart commentsAfter maybeThirdPart ->
                                    ( commentsBefore
                                        |> Rope.prependTo (secondPart |> Tuple.first)
                                        |> Rope.prependTo commentsAfter
                                        |> Rope.prependTo (maybeThirdPart |> Tuple.first)
                                    , Just { maybeThirdPart = maybeThirdPart |> Tuple.second, secondPart = secondPart |> Tuple.second }
                                    )
                                )
                                Layout.maybeLayout
                                pattern
                                Layout.maybeLayout
                                (ParserFast.oneOf2
                                    (ParserFast.symbol ")" ( Rope.empty, Nothing ))
                                    (ParserFast.symbolFollowedBy ","
                                        (ParserFast.map3
                                            (\commentsBefore thirdPart commentsAfter ->
                                                ( commentsBefore
                                                    |> Rope.prependTo (thirdPart |> Tuple.first)
                                                    |> Rope.prependTo commentsAfter
                                                , Just (thirdPart |> Tuple.second)
                                                )
                                            )
                                            Layout.maybeLayout
                                            pattern
                                            Layout.maybeLayout
                                            |> ParserFast.followedBySymbol ")"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


varPattern : Parser (WithComments (Node Pattern))
varPattern =
    Tokens.functionNameMapWithRange
        (\range var ->
            ( Rope.empty
            , Node range (VarPattern var)
            )
        )


numberPart : Parser (WithComments (Node Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n -> ( Rope.empty, Node range (IntPattern n) ))
        (\range n -> ( Rope.empty, Node range (HexPattern n) ))


charPattern : Parser (WithComments (Node Pattern))
charPattern =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            ( Rope.empty, Node range (CharPattern char) )
        )


listPattern : Parser (WithComments (Node Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    ( commentsBeforeElements
                    , Node range patternListEmpty
                    )

                Just elements ->
                    ( commentsBeforeElements |> Rope.prependTo (elements |> Tuple.first)
                    , Node range (ListPattern (elements |> Tuple.second))
                    )
        )
        (ParserFast.symbolFollowedBy "[" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    Just
                        ( head
                            |> Tuple.first
                            |> Rope.prependTo (tail |> Tuple.first)
                            |> Rope.prependTo commentsAfterHead
                        , (head |> Tuple.second) :: (tail |> Tuple.second)
                        )
                )
                pattern
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
                |> ParserFast.followedBySymbol "]"
            )
        )


patternListEmpty : Pattern
patternListEmpty =
    ListPattern []


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithConsumeArgs
        allPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


patternNotDirectlyComposing : Parser (WithComments (Node Pattern))
patternNotDirectlyComposing =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithoutConsumeArgs
        allPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


allPattern : Parser (WithComments (Node Pattern))
allPattern =
    ParserFast.symbolWithRange "_"
        (\range ->
            ( Rope.empty
            , Node range AllPattern
            )
        )


stringPattern : Parser (WithComments (Node Pattern))
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            ( Rope.empty
            , Node range (StringPattern string)
            )
        )


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.map2OrSucceed
        (\startName afterStartName ->
            case afterStartName of
                Nothing ->
                    Just ( [], startName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( startName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." Tokens.typeName)
        (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        Nothing


qualifiedPatternWithConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithConsumeArgs =
    ParserFast.map3
        (\(Node nameRange name) afterStartName argsReverse ->
            let
                range : Range
                range =
                    case argsReverse |> Tuple.second of
                        [] ->
                            nameRange

                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            ( afterStartName |> Rope.prependTo (argsReverse |> Tuple.first)
            , Node range
                (NamedPattern
                    name
                    (List.reverse (argsReverse |> Tuple.second))
                )
            )
        )
        qualifiedNameRefNode
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfterArg ->
                        ( arg |> Tuple.first |> Rope.prependTo commentsAfterArg
                        , arg |> Tuple.second
                        )
                    )
                    patternNotDirectlyComposing
                    Layout.optimisticLayout
                )
            )
        )


qualifiedNameRefNode : Parser (Node QualifiedNameRef)
qualifiedNameRefNode =
    ParserFast.map2WithRange
        (\range firstName after ->
            Node range
                (case after of
                    Nothing ->
                        { moduleName = [], name = firstName }

                    Just ( qualificationAfter, unqualified ) ->
                        { moduleName = firstName :: qualificationAfter, name = unqualified }
                )
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            ( Rope.empty
            , Node range
                (NamedPattern
                    (case after of
                        Nothing ->
                            { moduleName = [], name = firstName }

                        Just ( qualificationAfter, unqualified ) ->
                            { moduleName = firstName :: qualificationAfter, name = unqualified }
                    )
                    []
                )
            )
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


recordPattern : Parser (WithComments (Node Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            ( commentsBeforeElements |> Rope.prependTo (elements |> Tuple.first)
            , Node range (RecordPattern (elements |> Tuple.second))
            )
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    ( commentsAfterHead
                        |> Rope.prependTo (tail |> Tuple.first)
                    , head :: (tail |> Tuple.second)
                    )
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map3
                            (\beforeName name afterName ->
                                ( beforeName |> Rope.prependTo afterName
                                , name
                                )
                            )
                            Layout.maybeLayout
                            Tokens.functionNameNode
                            Layout.maybeLayout
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" ( Rope.empty, [] ))
        )
