module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

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
    CustomParser.map2
        (\x maybeComposedWithResult ->
            { comments =
                x.comments
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
        (Layout.maybeLayoutFollowedByWithComments
            maybeComposedWith
        )


maybeComposedWith : Parser (WithComments PatternComposedWith)
maybeComposedWith =
    CustomParser.oneOf
        [ CustomParser.map
            (\name ->
                { comments = name.comments
                , syntax = PatternComposedWithAs name.syntax
                }
            )
            (CustomParser.keywordFollowedBy "as"
                (Layout.maybeLayoutFollowedBy
                    (Node.parserCore Tokens.functionName)
                )
            )
        , CustomParser.map
            (\patternResult ->
                { comments = patternResult.comments
                , syntax = PatternComposedWithCons patternResult.syntax
                }
            )
            (CustomParser.symbolFollowedBy "::"
                (Layout.maybeLayoutFollowedByWithComments
                    pattern
                )
            )
        , CustomParser.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    CustomParser.symbolFollowedBy "("
        (Layout.maybeLayoutFollowedByWithComments
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (CustomParser.oneOf2
                (CustomParser.map2
                    (\headResult tailResult ->
                        { comments =
                            headResult.comments
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
                    (Layout.maybeLayoutFollowedByWithComments
                        (ParserWithComments.until
                            Tokens.parensEnd
                            (CustomParser.symbolFollowedBy ","
                                (Layout.maybeAroundBothSides pattern)
                            )
                        )
                    )
                )
                (CustomParser.symbol ")" { comments = Rope.empty, syntax = UnitPattern })
            )
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
    CustomParser.symbolFollowedBy "["
        (Layout.maybeLayoutFollowedByWithComments
            (CustomParser.oneOf2
                (CustomParser.symbol "]"
                    { comments = Rope.empty
                    , syntax = ListPattern []
                    }
                )
                (CustomParser.map3
                    (\head tail () ->
                        { comments =
                            head.comments |> Rope.prependTo tail.comments
                        , syntax = ListPattern (head.syntax :: tail.syntax)
                        }
                    )
                    pattern
                    (Layout.maybeLayoutFollowedByWithComments
                        (ParserWithComments.many
                            (CustomParser.symbolFollowedBy ","
                                (Layout.maybeAroundBothSides pattern)
                            )
                        )
                    )
                    Tokens.squareEnd
                )
            )
        )
        |> Node.parser


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


patternNotDirectlyComposing : Parser (WithComments (Node Pattern))
patternNotDirectlyComposing =
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
    CustomParser.orSucceed
        (CustomParser.map2
            (\startName afterStartName ->
                case afterStartName of
                    Nothing ->
                        Just ( [], startName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( startName :: qualificationAfter, unqualified )
            )
            (CustomParser.symbolFollowedBy "." Tokens.typeName)
            (CustomParser.lazy (\() -> maybeDotTypeNamesTuple))
        )
        Nothing


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
                patternNotDirectlyComposing
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
    CustomParser.symbolFollowedBy "{"
        (Layout.maybeLayoutFollowedByWithComments
            (CustomParser.oneOf2
                (CustomParser.map3
                    (\head tail () ->
                        { comments = tail.comments
                        , syntax = RecordPattern (head :: tail.syntax)
                        }
                    )
                    (Node.parserCore Tokens.functionName)
                    (Layout.maybeLayoutFollowedByWithComments
                        (ParserWithComments.many
                            (CustomParser.map2
                                (\name afterName ->
                                    { comments = name.comments |> Rope.prependTo afterName
                                    , syntax = name.syntax
                                    }
                                )
                                (CustomParser.symbolFollowedBy ","
                                    (Layout.maybeLayoutFollowedBy
                                        (Node.parserCore Tokens.functionName)
                                    )
                                )
                                Layout.maybeLayout
                            )
                        )
                    )
                    Tokens.curlyEnd
                )
                (CustomParser.symbol "}"
                    { comments = Rope.empty
                    , syntax = RecordPattern []
                    }
                )
            )
        )
        |> Node.parser
