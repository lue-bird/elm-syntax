module ParserWithComments exposing
    ( Comments
    , WithComments
    , many
    , manyWithoutReverse
    , sepBy
    , sepBy1
    )

import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|=), Parser)
import Parser.Extra
import Rope exposing (Rope)


type alias WithComments res =
    { comments : Comments, syntax : res }


type alias Comments =
    Rope (Node String)


maybeMap : (a -> b) -> b -> Parser (WithComments a) -> Parser (WithComments b)
maybeMap onJust onNothing p =
    Core.oneOf
        [ p
            |> Core.map
                (\syntaxAndComments ->
                    { comments = syntaxAndComments.comments
                    , syntax = onJust syntaxAndComments.syntax
                    }
                )
        , Core.succeed { comments = Rope.empty, syntax = onNothing }
        ]


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    let
        manyWithoutReverseStep :
            ( List Comments, List a )
            ->
                Core.Parser
                    (Core.Step
                        ( List Comments, List a )
                        { comments : Comments, syntax : List a }
                    )
        manyWithoutReverseStep ( commentsSoFar, items ) =
            Core.oneOf
                [ p
                    |> Core.map
                        (\pResult ->
                            Core.Loop
                                ( pResult.comments :: commentsSoFar
                                , pResult.syntax :: items
                                )
                        )
                , Core.lazy
                    (\() ->
                        Core.succeed
                            (Core.Done
                                { comments = Rope.flatFromList (List.reverse commentsSoFar)
                                , syntax = List.reverse items
                                }
                            )
                    )
                ]
    in
    Core.loop ( [], [] ) manyWithoutReverseStep


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithoutReverse : Parser (WithComments a) -> Parser (WithComments (List a))
manyWithoutReverse p =
    let
        manyWithoutReverseStep :
            ( List Comments, List a )
            ->
                Core.Parser
                    (Core.Step
                        ( List Comments, List a )
                        { comments : Comments, syntax : List a }
                    )
        manyWithoutReverseStep ( commentsSoFar, items ) =
            Core.oneOf
                [ p
                    |> Core.map
                        (\pResult ->
                            Core.Loop
                                ( pResult.comments :: commentsSoFar
                                , pResult.syntax :: items
                                )
                        )
                , Core.lazy
                    (\() ->
                        Core.succeed
                            (Core.Done
                                { comments = Rope.flatFromList (List.reverse commentsSoFar)
                                , syntax = items
                                }
                            )
                    )
                ]
    in
    Core.loop ( [], [] ) manyWithoutReverseStep


sepBy : String -> Parser (WithComments a) -> Parser (WithComments (List a))
sepBy sep p =
    maybeMap identity
        []
        (sepBy1 sep p)


sepBy1 : String -> Parser (WithComments a) -> Parser (WithComments (List a))
sepBy1 sep p =
    Core.map
        (\head ->
            \tail ->
                { comments = Rope.flatFromList [ head.comments, tail.comments ]
                , syntax = head.syntax :: tail.syntax
                }
        )
        p
        |= many (Core.symbol sep |> Parser.Extra.continueWith p)
