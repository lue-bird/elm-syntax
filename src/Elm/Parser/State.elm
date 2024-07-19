module Elm.Parser.State exposing
    ( State
    , addComment
    , currentIndent
    , emptyState
    , expectedColumn
    , getComments
    , popIndent
    , pushIndent
    , storedColumns
    )

import Elm.Syntax.Node exposing (Node)


type State
    = State
        { indents : List Int
        , comments : List (Node String)
        }


emptyState : State
emptyState =
    State
        { indents = []
        , comments = []
        }


storedColumns : State -> List Int
storedColumns (State { indents }) =
    indents


currentIndent : State -> Maybe Int
currentIndent (State { indents }) =
    List.head indents


expectedColumn : State -> Int
expectedColumn (State { indents }) =
    case indents of
        [] ->
            1

        head :: _ ->
            head


pushIndent : Int -> State -> State
pushIndent col (State s) =
    State { s | indents = col :: s.indents }


popIndent : State -> State
popIndent (State s) =
    State
        { s
            | indents =
                case s.indents of
                    [] ->
                        []

                    _ :: restIndents ->
                        restIndents
        }


addComment : Node String -> State -> State
addComment pair (State s) =
    State { s | comments = pair :: s.comments }


getComments : State -> List (Node String)
getComments (State s) =
    List.reverse s.comments
