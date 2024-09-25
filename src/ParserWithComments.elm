module ParserWithComments exposing
    ( Comments
    , WithComments
    , many
    , manyWithoutReverse
    , until
    )

import Elm.Syntax.Node exposing (Node)
import ParserFast exposing (Parser)
import Rope exposing (Rope)


type alias WithComments res =
    ( Comments, res )


type alias Comments =
    Rope (Node String)


until : Parser () -> Parser (WithComments a) -> Parser (WithComments (List a))
until end element =
    ParserFast.loopUntil
        end
        element
        ( Rope.empty, [] )
        (\( pResultComments, pResultSyntax ) ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo pResultComments
            , pResultSyntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar
            , List.reverse itemsSoFar
            )
        )


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    ParserFast.loopWhileSucceeds p
        ( Rope.empty, [] )
        (\( pResultComments, pResultSyntax ) ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo pResultComments
            , pResultSyntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar
            , List.reverse itemsSoFar
            )
        )


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithoutReverse : Parser (WithComments a) -> Parser (WithComments (List a))
manyWithoutReverse p =
    ParserFast.loopWhileSucceeds p
        ( Rope.empty, [] )
        (\( pResultComments, pResultSyntax ) ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo pResultComments
            , pResultSyntax :: itemsSoFar
            )
        )
        Basics.identity
