module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser as Core
import Parser.Extra


importDefinition : Parser State (Node Import)
importDefinition =
    Combine.succeed (\start -> \mod -> importInnerParseAsDefinition start mod)
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.importToken
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore moduleName
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.andThen identity
        |> Combine.ignore Layout.optimisticLayout


importInnerAsDefinition : Parser State (Node ModuleName)
importInnerAsDefinition =
    Tokens.asToken
        |> Combine.ignoreFromCore Layout.layout
        |> Combine.continueWithCore (Tokens.typeName |> Core.map List.singleton |> Node.parserCore)


importInnerParseExposingDefinition : Node ModuleName -> Maybe (Node ModuleName) -> Parser State Import
importInnerParseExposingDefinition mod asDef =
    Combine.oneOf
        [ Node.parser exposeDefinition
            |> Combine.map (\exposing_ -> { moduleName = mod, moduleAlias = asDef, exposingList = Just exposing_ })
        , Combine.succeed { moduleName = mod, moduleAlias = asDef, exposingList = Nothing }
        ]


importInnerParseAsDefinition : Location -> Node ModuleName -> Parser State (Node Import)
importInnerParseAsDefinition start mod =
    Combine.oneOf
        [ importInnerAsDefinition
            |> Combine.ignore Layout.optimisticLayout
            |> Combine.andThen (\alias_ -> importInnerParseExposingDefinition mod (Just alias_))
        , importInnerParseExposingDefinition mod Nothing
        ]
        |> Combine.map (\imp -> setupNode start imp)


setupNode : Location -> Import -> Node Import
setupNode start imp =
    let
        endRange : Range
        endRange =
            case imp.moduleAlias of
                Just (Node range _) ->
                    range

                Nothing ->
                    case imp.exposingList of
                        Just (Node range _) ->
                            range

                        Nothing ->
                            Node.range imp.moduleName
    in
    Node
        { start = start, end = endRange.end }
        imp
