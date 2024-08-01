module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


moduleDefinition : Parser (WithComments Module)
moduleDefinition =
    Parser.oneOf
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser (WithComments ( String, Node String ))
effectWhereClause =
    Parser.map
        (\fnName ->
            \commentsAfterFnName ->
                \typeName_ ->
                    { comments = commentsAfterFnName |> Rope.prependTo typeName_.comments
                    , syntax = ( fnName, typeName_.syntax )
                    }
        )
        Tokens.functionName
        |= Layout.maybeLayoutUntilIgnored Tokens.equal
        |= Layout.maybeLayoutUntil (Node.parserCore Tokens.typeName)


whereBlock : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
whereBlock =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (ParserWithComments.sepBy1 ","
                (Layout.maybeAroundBothSides effectWhereClause)
                |> Parser.map
                    (\pairs ->
                        { comments = pairs.comments
                        , syntax =
                            { command =
                                pairs.syntax
                                    |> List.Extra.find (\( fnName, _ ) -> fnName == "command")
                                    |> Maybe.map Tuple.second
                            , subscription =
                                pairs.syntax
                                    |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription")
                                    |> Maybe.map Tuple.second
                            }
                        }
                    )
            )
    )
        |. Tokens.curlyEnd


effectWhereClauses : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
effectWhereClauses =
    Tokens.whereToken
        |> Parser.Extra.continueWith
            (Layout.maybeLayoutUntilWithComments whereBlock)


effectModuleDefinition : Parser (WithComments Module)
effectModuleDefinition =
    (Parser.keyword "effect"
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterEffect ->
                    \name ->
                        \whereClauses ->
                            \exp ->
                                { comments =
                                    commentsAfterEffect
                                        |> Rope.prependTo name.comments
                                        |> Rope.prependTo whereClauses.comments
                                        |> Rope.prependTo exp.comments
                                , syntax =
                                    EffectModule
                                        { moduleName = name.syntax
                                        , exposingList = exp.syntax
                                        , command = whereClauses.syntax.command
                                        , subscription = whereClauses.syntax.subscription
                                        }
                                }
                )
                Layout.maybeLayout
            )
    )
        |. Tokens.moduleToken
        |= Layout.maybeLayoutUntil moduleName
        |= Layout.maybeLayoutUntilWithComments effectWhereClauses
        |= Layout.maybeLayoutUntilWithComments
            (Node.parser exposeDefinition)


normalModuleDefinition : Parser (WithComments Module)
normalModuleDefinition =
    (Tokens.moduleToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\moduleName ->
                    \exposingList ->
                        { comments =
                            moduleName.comments
                                |> Rope.prependTo exposingList.comments
                        , syntax =
                            NormalModule
                                { moduleName = moduleName.syntax
                                , exposingList = exposingList.syntax
                                }
                        }
                )
                (Layout.maybeLayoutUntil moduleName)
            )
    )
        |= Layout.maybeLayoutUntilWithComments (Node.parser exposeDefinition)


portModuleDefinition : Parser (WithComments Module)
portModuleDefinition =
    (Tokens.portToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterPort ->
                    \moduleName ->
                        \exposingList ->
                            { comments =
                                commentsAfterPort
                                    |> Rope.prependTo moduleName.comments
                                    |> Rope.prependTo exposingList.comments
                            , syntax =
                                PortModule
                                    { moduleName = moduleName.syntax
                                    , exposingList = exposingList.syntax
                                    }
                            }
                )
                Layout.maybeLayout
            )
    )
        |. Tokens.moduleToken
        |= Layout.maybeLayoutUntil moduleName
        |= Layout.maybeLayoutUntilWithComments
            (Node.parser exposeDefinition)
