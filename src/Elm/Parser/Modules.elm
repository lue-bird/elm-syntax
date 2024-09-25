module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import List.Extra
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


moduleDefinition : Parser (WithComments (Node Module))
moduleDefinition =
    ParserFast.oneOf3
        normalModuleDefinition
        portModuleDefinition
        effectModuleDefinition


effectWhereClause : Parser (WithComments ( String, Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual typeName_ ->
            ( commentsAfterFnName |> Rope.prependTo commentsAfterEqual
            , ( fnName, typeName_ )
            )
        )
        Tokens.functionName
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        Tokens.typeNameNode


whereBlock : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
whereBlock =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map4
            (\commentsBeforeHead head commentsAfterHead tail ->
                let
                    pairs : List ( String, Node String )
                    pairs =
                        (head |> Tuple.second) :: (tail |> Tuple.second)
                in
                ( commentsBeforeHead
                    |> Rope.prependTo (head |> Tuple.first)
                    |> Rope.prependTo commentsAfterHead
                    |> Rope.prependTo (tail |> Tuple.first)
                , { command =
                        pairs
                            |> List.Extra.find (\( fnName, _ ) -> fnName == "command")
                            |> Maybe.map Tuple.second
                  , subscription =
                        pairs
                            |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription")
                            |> Maybe.map Tuple.second
                  }
                )
            )
            Layout.maybeLayout
            effectWhereClause
            Layout.maybeLayout
            (ParserWithComments.many
                (ParserFast.symbolFollowedBy "," (Layout.maybeAroundBothSides effectWhereClause))
            )
        )
        |> ParserFast.followedBySymbol "}"


effectWhereClauses : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            ( commentsBefore |> Rope.prependTo (whereResult |> Tuple.first)
            , whereResult |> Tuple.second
            )
        )
        (ParserFast.keywordFollowedBy "where" Layout.maybeLayout)
        whereBlock


effectModuleDefinition : Parser (WithComments (Node Module))
effectModuleDefinition =
    ParserFast.map7WithRange
        (\range commentsAfterEffect commentsAfterModule name commentsAfterName whereClauses commentsAfterWhereClauses exp ->
            ( commentsAfterEffect
                |> Rope.prependTo commentsAfterModule
                |> Rope.prependTo commentsAfterName
                |> Rope.prependTo (whereClauses |> Tuple.first)
                |> Rope.prependTo commentsAfterWhereClauses
                |> Rope.prependTo (exp |> Tuple.first)
            , Node range
                (EffectModule
                    { moduleName = name
                    , exposingList = exp |> Tuple.second
                    , command = (whereClauses |> Tuple.second).command
                    , subscription = (whereClauses |> Tuple.second).subscription
                    }
                )
            )
        )
        (ParserFast.keywordFollowedBy "effect" Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        moduleName
        Layout.maybeLayout
        effectWhereClauses
        Layout.maybeLayout
        exposeDefinition


normalModuleDefinition : Parser (WithComments (Node Module))
normalModuleDefinition =
    ParserFast.map4WithRange
        (\range commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            ( commentsAfterModule
                |> Rope.prependTo commentsAfterModuleName
                |> Rope.prependTo (exposingList |> Tuple.first)
            , Node range
                (NormalModule
                    { moduleName = moduleName
                    , exposingList = exposingList |> Tuple.second
                    }
                )
            )
        )
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        moduleName
        Layout.maybeLayout
        exposeDefinition


portModuleDefinition : Parser (WithComments (Node Module))
portModuleDefinition =
    ParserFast.map5WithRange
        (\range commentsAfterPort commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            ( commentsAfterPort
                |> Rope.prependTo commentsAfterModule
                |> Rope.prependTo commentsAfterModuleName
                |> Rope.prependTo (exposingList |> Tuple.first)
            , Node range
                (PortModule { moduleName = moduleName, exposingList = exposingList |> Tuple.second })
            )
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        moduleName
        Layout.maybeLayout
        exposeDefinition
