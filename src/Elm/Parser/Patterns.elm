module Elm.Parser.Patterns exposing (pattern)

import CustomParser exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser (WithComments (Node Pattern))
pattern =
    CustomParser.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
composablePatternTryToCompose =
    CustomParser.map3
        (\x commentsAfterLeft maybeComposedWithResult ->
            { comments =
                x.comments
                    |> Rope.prependTo commentsAfterLeft
                    |> Rope.prependTo maybeComposedWithResult.comments
            , syntax =
                case maybeComposedWithResult.syntax of
                    PatternComposedWithNothing () ->
                        x.syntax

                    PatternComposedWithAs anotherName ->
                        Node.combine Pattern.AsPattern x.syntax anotherName

                    PatternComposedWithCons y ->
                        Node.combine Pattern.UnConsPattern x.syntax y
            }
        )
        composablePattern
        Layout.maybeLayout
        maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    CustomParser.oneOf
        [ CustomParser.map2
            (\commentsAfterAs name ->
                { comments = commentsAfterAs
                , syntax = PatternComposedWithAs name
                }
            )
            (CustomParser.keywordFollowedBy "as" Layout.maybeLayout)
            (Node.parserCore Tokens.functionName)
        , CustomParser.map2
            (\commentsAfterCons patternResult ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterCons
                , syntax = PatternComposedWithCons patternResult.syntax
                }
            )
            (CustomParser.symbolFollowedBy "::" Layout.maybeLayout)
            pattern
        , CustomParser.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    CustomParser.map2
        (\commentsBeforeHead contentResult ->
            { comments =
                commentsBeforeHead
                    |> Rope.prependTo contentResult.comments
            , syntax = contentResult.syntax
            }
        )
        (CustomParser.symbolFollowedBy "(" Layout.maybeLayout)
        -- yes, (  ) is a valid pattern but not a valid type or expression
        (CustomParser.oneOf
            [ CustomParser.map3
                (\headResult commentsAfterHead tailResult ->
                    { comments =
                        headResult.comments
                            |> Rope.prependTo commentsAfterHead
                            |> Rope.prependTo tailResult.comments
                    , syntax =
                        case tailResult.syntax of
                            [] ->
                                ParenthesizedPattern headResult.syntax

                            _ ->
                                TuplePattern (headResult.syntax :: tailResult.syntax)
                    }
                )
                pattern
                Layout.maybeLayout
                (ParserWithComments.until
                    Tokens.parensEnd
                    (CustomParser.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
            , CustomParser.symbol ")" { comments = Rope.empty, syntax = UnitPattern }
            ]
        )
        |> Node.parser


varPattern : Parser (WithComments (Node Pattern))
varPattern =
    Tokens.functionName
        |> CustomParser.mapWithStartAndEndPosition
            (\start var end ->
                { comments = Rope.empty
                , syntax =
                    Node { start = start, end = end } (VarPattern var)
                }
            )


numberPart : Parser (WithComments (Node Pattern))
numberPart =
    Elm.Parser.Numbers.intOrHex
        (\n -> { comments = Rope.empty, syntax = IntPattern n })
        (\n -> { comments = Rope.empty, syntax = HexPattern n })
        |> Node.parser


charPattern : Parser (WithComments (Node Pattern))
charPattern =
    Tokens.characterLiteral
        |> CustomParser.mapWithStartAndEndPosition
            (\start char end ->
                { comments = Rope.empty, syntax = Node { start = start, end = end } (CharPattern char) }
            )


listPattern : Parser (WithComments (Node Pattern))
listPattern =
    CustomParser.map2
        (\commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = patternListEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                    , syntax = ListPattern elements.syntax
                    }
        )
        (CustomParser.symbolFollowedBy "[" Layout.maybeLayout)
        (CustomParser.oneOf
            [ CustomParser.symbol "]" Nothing
            , CustomParser.map4
                (\head commentsAfterHead tail () ->
                    Just
                        { comments =
                            head.comments
                                |> Rope.prependTo tail.comments
                                |> Rope.prependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
                )
                pattern
                Layout.maybeLayout
                (ParserWithComments.many
                    (CustomParser.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
                Tokens.squareEnd
            ]
        )
        |> Node.parser


patternListEmpty : Pattern
patternListEmpty =
    ListPattern []


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    CustomParser.oneOf
        [ varPattern
        , qualifiedPatternWithConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        , stringPattern
        , listPattern
        , numberPart
        , charPattern
        ]


qualifiedPatternArg : Parser (WithComments (Node Pattern))
qualifiedPatternArg =
    CustomParser.oneOf
        [ varPattern
        , qualifiedPatternWithoutConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        , stringPattern
        , listPattern
        , numberPart
        , charPattern
        ]


allPattern : Parser (WithComments (Node Pattern))
allPattern =
    CustomParser.symbol "_" { comments = Rope.empty, syntax = AllPattern }
        |> Node.parser


unitPattern : Parser (WithComments (Node Pattern))
unitPattern =
    CustomParser.symbol "()" { comments = Rope.empty, syntax = UnitPattern }
        |> Node.parser


stringPattern : Parser (WithComments (Node Pattern))
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteral
        |> CustomParser.mapWithStartAndEndPosition
            (\start string end ->
                { comments = Rope.empty
                , syntax =
                    Node { start = start, end = end } (StringPattern string)
                }
            )


maybeDotTypeNamesTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    CustomParser.oneOf
        [ CustomParser.map2
            (\startName afterStartName ->
                case afterStartName of
                    Nothing ->
                        Just ( [], startName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( startName :: qualificationAfter, unqualified )
            )
            (CustomParser.symbolFollowedBy "." Tokens.typeName)
            (CustomParser.lazy (\() -> maybeDotTypeNamesTuple))
        , CustomParser.succeed Nothing
        ]


qualifiedPatternWithConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithConsumeArgs =
    CustomParser.map3
        (\startName afterStartName args ->
            { comments = args.comments
            , syntax =
                NamedPattern
                    (case afterStartName of
                        Nothing ->
                            { moduleName = [], name = startName }

                        Just ( qualificationAfter, unqualified ) ->
                            { moduleName = startName :: qualificationAfter, name = unqualified }
                    )
                    args.syntax
            }
        )
        Tokens.typeName
        maybeDotTypeNamesTuple
        (ParserWithComments.many
            (CustomParser.map2
                (\commentsBefore arg ->
                    { comments = arg.comments |> Rope.prependTo commentsBefore
                    , syntax = arg.syntax
                    }
                )
                (Layout.maybeLayout |> CustomParser.backtrackable)
                qualifiedPatternArg
            )
        )
        |> Node.parser


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithoutConsumeArgs =
    CustomParser.mapWithStartAndEndPosition
        (\start name end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end } (NamedPattern name [])
            }
        )
        (CustomParser.map2
            (\firstName after ->
                case after of
                    Nothing ->
                        { moduleName = [], name = firstName }

                    Just ( qualificationAfter, unqualified ) ->
                        { moduleName = firstName :: qualificationAfter, name = unqualified }
            )
            Tokens.typeName
            maybeDotTypeNamesTuple
        )


recordPattern : Parser (WithComments (Node Pattern))
recordPattern =
    CustomParser.map2
        (\commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = patternRecordEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                    , syntax = RecordPattern elements.syntax
                    }
        )
        (CustomParser.symbolFollowedBy "{" Layout.maybeLayout)
        (CustomParser.oneOf
            [ CustomParser.map4
                (\head commentsAfterHead tail () ->
                    Just
                        { comments =
                            commentsAfterHead
                                |> Rope.prependTo tail.comments
                        , syntax = head :: tail.syntax
                        }
                )
                (Node.parserCore Tokens.functionName)
                Layout.maybeLayout
                (ParserWithComments.many
                    (CustomParser.map3
                        (\beforeName name afterName ->
                            { comments = beforeName |> Rope.prependTo afterName
                            , syntax = name
                            }
                        )
                        (CustomParser.symbolFollowedBy "," Layout.maybeLayout)
                        (Node.parserCore Tokens.functionName)
                        Layout.maybeLayout
                    )
                )
                Tokens.curlyEnd
            , CustomParser.symbol "}" Nothing
            ]
        )
        |> Node.parser


patternRecordEmpty : Pattern
patternRecordEmpty =
    RecordPattern []
