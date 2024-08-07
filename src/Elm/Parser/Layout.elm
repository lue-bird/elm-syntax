module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , onTopIndentation
    , optimisticLayout
    , optimisticLayoutWith
    , positivelyIndented
    )

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Whitespace as Whitespace
import Parser.Extra


anyComment : Combine.Parser State ()
anyComment =
    Combine.oneOf
        [ Comments.singleLineComment
        , Comments.multilineComment
        ]


layout : Parser State ()
layout =
    Combine.many1Ignore
        (Combine.oneOf
            [ anyComment
            , Parser.Extra.many1Ignore Whitespace.realNewLine
                |> Combine.ignoreFromCore
                    (Combine.oneOf
                        [ Whitespace.many1Spaces
                        , anyComment
                        ]
                    )
            , Whitespace.many1Spaces
            ]
        )
        |> Combine.continueWith
            (verifyIndent (\stateIndent current -> stateIndent < current)
                (\stateIndent current -> "Expected indent larger than " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)
            )


optimisticLayoutWith : (() -> a) -> (() -> Parser State a) -> Parser State a
optimisticLayoutWith onStrict onIndented =
    optimisticLayout
        |> Combine.continueWith (compute onStrict onIndented)


optimisticLayout : Parser State ()
optimisticLayout =
    Combine.manyIgnore
        (Combine.oneOf
            [ anyComment
            , Parser.Extra.many1Ignore Whitespace.realNewLine
                |> Combine.ignoreFromCore
                    (Combine.oneOf
                        [ Whitespace.many1Spaces
                        , anyComment
                        , Combine.succeed ()
                        ]
                    )
            , Whitespace.many1Spaces
            ]
        )


compute : (() -> a) -> (() -> Parser State a) -> Parser State a
compute onStrict onIndented =
    Combine.withLocation
        (\l ->
            if l.column == 1 then
                Combine.succeed (onStrict ())

            else
                Combine.withState
                    (\state ->
                        if List.member l.column (State.storedColumns state) then
                            Combine.succeed (onStrict ())

                        else
                            onIndented ()
                    )
        )


layoutStrict : Parser State ()
layoutStrict =
    Combine.many1Ignore
        (Combine.oneOf
            [ anyComment
            , Parser.Extra.many1Ignore Whitespace.realNewLine
                |> Combine.fromCore
            , Whitespace.many1Spaces
            ]
        )
        |> Combine.continueWith
            (verifyIndent (\stateIndent current -> stateIndent == current)
                (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)
            )


onTopIndentation : Parser State ()
onTopIndentation =
    Combine.withState
        (\state ->
            Combine.withLocation
                (\{ column } ->
                    if State.currentIndent state == Just column then
                        Combine.succeed ()

                    else
                        Combine.problem "must be on top indentation"
                )
        )


positivelyIndented : Parser State ()
positivelyIndented =
    verifyIndent (\stateIndent current -> stateIndent < current) (\_ _ -> "must be positively indented")


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser State ()
verifyIndent verify failMessage =
    Combine.withState
        (\state ->
            Combine.withLocation
                (\{ column } ->
                    let
                        expectedColumn : Int
                        expectedColumn =
                            State.expectedColumn state
                    in
                    if verify expectedColumn column then
                        Combine.succeed ()

                    else
                        Combine.problem (failMessage expectedColumn column)
                )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    Combine.maybeIgnore layout
        |> Combine.continueWith x
        |> Combine.ignore (Combine.maybeIgnore layout)
