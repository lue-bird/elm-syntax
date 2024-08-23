module ParserFast.Advanced exposing
    ( Parser, run
    , number, symbol1, symbol1Backtrackable, symbol2, symbol3, symbol, symbol1FollowedBy, symbol2FollowedBy, symbolFollowedBy, keyword, keywordFollowedBy, whileMap, ifFollowedByWhile, ifFollowedByWhileExcept, anyChar, end
    , succeed, problem, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, validate
    , orSucceed, oneOf2, oneOf2OrSucceed, oneOf2Map, oneOf3, oneOf4, oneOf
    , loopWhileSucceeds, loopUntil
    , chompWhileWhitespaceFollowedBy, nestableMultiComment
    , withIndent, withIndentSetToColumn
    , columnAndThen, columnIndentAndThen, validateEndColumnIndentation, validateEndColumnIndentationBacktrackable, mapWithStartPosition, mapWithEndPosition, mapWithStartAndEndPosition
    , andThenWithPreviousChar, andThenWithRemaining
    )

{-|

@docs Parser, run

@docs number, symbol1, symbol1Backtrackable, symbol2, symbol3, symbol, symbol1FollowedBy, symbol2FollowedBy, symbolFollowedBy, keyword, keywordFollowedBy, whileMap, ifFollowedByWhile, ifFollowedByWhileExcept, anyChar, end


# Flow

@docs succeed, problem, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, validate

@docs orSucceed, oneOf2, oneOf2OrSucceed, oneOf2Map, oneOf3, oneOf4, oneOf

@docs loopWhileSucceeds, loopUntil


# Whitespace

@docs chompWhileWhitespaceFollowedBy, nestableMultiComment


# Indentation and positions

@docs withIndent, withIndentSetToColumn
@docs columnAndThen, columnIndentAndThen, validateEndColumnIndentation, validateEndColumnIndentationBacktrackable, mapWithStartPosition, mapWithEndPosition, mapWithStartAndEndPosition


# Look behind and ahead

@docs andThenWithPreviousChar, andThenWithRemaining

-}

import Char
import Char.Extra
import Parser.Advanced exposing ((|.), (|=))
import Set



-- PARSERS


{-| An advanced `ParserFast` gives a way to improve your error messages: `problem`;
Instead of all errors being a `String`, you can create a
custom type like `type Problem = BadIndent | BadKeyword String` and track
problems much more precisely.

I recommend starting with the simpler [`ParserFast`](ParserFast) module though, and
when you feel comfortable and want better error messages, you can create a type
alias like this:

    import Parser.Advanced

    type alias MyParser a =
        Parser.Advanced.Parser Context Problem a

    type Context
        = Definition String
        | List
        | Record

    type Problem
        = BadIndent
        | BadKeyword String

All of the functions from `ParserFast` should exist in `ParserFast.Advanced` in some
form, allowing you to switch over pretty easily.

-}
type Parser problem value
    = Parser (State -> PStep problem value)


type PStep problem value
    = Good Bool value State
    | Bad Bool (RopeFilled (DeadEnd problem)) ()


type alias State =
    { previousChar : Maybe Char
    , remaining : List Char
    , indent : Int
    , row : Int
    , col : Int
    }



-- RUN


{-| This works just like [`ParserFast.run`](ParserFast#run).
The only difference is that when it fails, it has much more precise information
for each dead end.
-}
run : Parser x a -> String -> Result (List (DeadEnd x)) a
run (Parser parse) src =
    case parse { remaining = String.toList src, previousChar = Nothing, indent = 1, row = 1, col = 1 } of
        Good _ value _ ->
            Ok value

        Bad _ deadEnds () ->
            Err (ropeFilledToList deadEnds [])
 



-- PROBLEMS


{-| Say you are parsing a function named `viewHealthData` that contains a list.
You might get a `DeadEnd` like this:

    { row = 18
    , col = 22
    , problem = UnexpectedComma
    , contextStack =
        [ { row = 14
          , col = 1
          , context = Definition "viewHealthData"
          }
        , { row = 15
          , col = 4problem
          , context = List
          }
        ]
    }

We have a ton of information here! So in the error message, we can say that “I
ran into an issue when parsing a list in the definition of `viewHealthData`. It
looks like there is an extra comma.” Or maybe something even better!

Furthermore, many parsers just put a mark where the problem manifested. By
tracking the `row` and `col` of the context, we can show a much larger region
as a way of indicating “I thought I was parsing this thing that starts over
here.” Otherwise you can get very confusing error messages on a missing `]` or
`}` or `)` because “I need more indentation” on something unrelated.

**Note:** Rows and columns are counted like a text editor. The beginning is `row=1`
and `col=1`. The `col` increments as characters are chomped. When a `\n` is chomped,
`row` is incremented and `col` starts over again at `1`.

-}
type alias DeadEnd problem =
    { row : Int
    , col : Int
    , problem : problem
    }


type RopeFilled a
    = One a ()
    | Append (RopeFilled a) (RopeFilled a)


fromState : State -> x -> RopeFilled (DeadEnd x)
fromState s x =
    One { row = s.row, col = s.col, problem = x } ()


ropeFilledToList : RopeFilled x -> List x -> List x
ropeFilledToList ropeFilled list =
    case ropeFilled of
        One x () ->
            x :: list

        Append ropefilled1 ropefilled2 ->
            ropeFilledToList ropefilled1 (ropeFilledToList ropefilled2 list)


succeed : a -> Parser x a
succeed a =
    Parser (\s -> Good False a s)


problem : x -> Parser x a
problem x =
    Parser (\s -> Bad False (fromState s x) ())


map : (a -> b) -> Parser x a -> Parser x b
map func (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good committed a s1 ->
                    Good committed (func a) s1

                Bad committed x () ->
                    Bad committed x ()
        )


map2 : (a -> b -> value) -> Parser x a -> Parser x b -> Parser x value
map2 func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            Good (c1 || c2) (func a b) s2
        )


map3 : (a -> b -> c -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x value
map3 func (Parser parseA) (Parser parseB) (Parser parseC) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    Good (c1 || c2 || c3) (func a b c) s3
        )


map4 : (a -> b -> c -> d -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x value
map4 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            Good (c1 || c2 || c3 || c4) (func a b c d) s4
        )


map5 : (a -> b -> c -> d -> e -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x value
map5 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    Good (c1 || c2 || c3 || c4 || c5) (func a b c d e) s5
        )


map6 : (a -> b -> c -> d -> e -> f -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x value
map6 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            Good (c1 || c2 || c3 || c4 || c5 || c6) (func a b c d e f) s6
        )


map7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x value
map7 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            case parseG s6 of
                                                                Bad c7 x () ->
                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7) x ()

                                                                Good c7 g s7 ->
                                                                    Good (c1 || c2 || c3 || c4 || c5 || c6 || c7) (func a b c d e f g) s7
        )


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x h -> Parser x value
map8 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            case parseG s6 of
                                                                Bad c7 x () ->
                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7) x ()

                                                                Good c7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad c8 x () ->
                                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8) x ()

                                                                        Good c8 h s8 ->
                                                                            Good (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8) (func a b c d e f g h) s8
        )


map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x h -> Parser x i -> Parser x value
map9 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) (Parser parseI) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            case parseG s6 of
                                                                Bad c7 x () ->
                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7) x ()

                                                                Good c7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad c8 x () ->
                                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8) x ()

                                                                        Good c8 h s8 ->
                                                                            case parseI s8 of
                                                                                Bad c9 x () ->
                                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8 || c9) x ()

                                                                                Good c9 i s9 ->
                                                                                    Good (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8 || c9) (func a b c d e f g h i) s9
        )


validate : (a -> Bool) -> x -> Parser x a -> Parser x a
validate isOkay problemOnNotOkay (Parser parseA) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                (Good committed a s1) as good ->
                    if isOkay a then
                        good

                    else
                        Bad committed (fromState s1 problemOnNotOkay) ()
        )


columnAndThen : (Int -> Parser x a) -> Parser x a
columnAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.col
            in
            parse s
        )


columnIndentAndThen : (Int -> Int -> Parser x a) -> Parser x a
columnIndentAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.col s.indent
            in
            parse s
        )


validateEndColumnIndentation : (Int -> Int -> Bool) -> x -> Parser x a -> Parser x a
validateEndColumnIndentation isOkay problemOnIsNotOkay (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                (Good committed _ s1) as good ->
                    if isOkay s1.col s1.indent then
                        good

                    else
                        Bad committed (fromState s1 problemOnIsNotOkay) ()

                bad ->
                    bad
        )


validateEndColumnIndentationBacktrackable : (Int -> Int -> Bool) -> x -> Parser x a -> Parser x a
validateEndColumnIndentationBacktrackable isOkay problemOnIsNotOkay (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good _ res s1 ->
                    if isOkay s1.col s1.indent then
                        Good False res s1

                    else
                        Bad False (fromState s1 problemOnIsNotOkay) ()

                Bad _ x () ->
                    Bad False x ()
        )


andThenWithRemaining : (List Char -> Parser x a) -> Parser x a
andThenWithRemaining callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.remaining
            in
            parse s
        )


andThenWithPreviousChar : (Maybe Char -> Parser x res) -> Parser x res
andThenWithPreviousChar callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.previousChar
            in
            parse s
        )


lazy : (() -> Parser x a) -> Parser x a
lazy thunk =
    Parser
        (\s ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse s
        )


oneOf2 : Parser x a -> Parser x a -> Parser x a
oneOf2 (Parser attemptFirst) (Parser attemptSecond) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    Bad False (Append firstX secondX) ()
        )


oneOf3 : Parser x a -> Parser x a -> Parser x a -> Parser x a
oneOf3 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX ()) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                Bad False (Append firstX (Append secondX thirdX)) ()
        )


oneOf4 : Parser x a -> Parser x a -> Parser x a -> Parser x a -> Parser x a
oneOf4 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) (Parser attemptFourth) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX ()) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                case attemptFourth s of
                                                    (Good _ _ _) as fourthGood ->
                                                        fourthGood

                                                    (Bad fourthCommitted fourthX ()) as fourthBad ->
                                                        if fourthCommitted then
                                                            fourthBad

                                                        else
                                                            Bad False (Append firstX (Append secondX (Append thirdX fourthX))) ()
        )


oneOf2Map :
    (first -> choice)
    -> Parser x first
    -> (second -> choice)
    -> Parser x second
    -> Parser x choice
oneOf2Map firstToChoice (Parser attemptFirst) secondToChoice (Parser attemptSecond) =
    Parser
        (\s ->
            case attemptFirst s of
                Good firstCommitted first s1 ->
                    Good firstCommitted (firstToChoice first) s1

                Bad firstCommitted firstX () ->
                    if firstCommitted then
                        Bad firstCommitted firstX ()

                    else
                        case attemptSecond s of
                            Good secondCommitted second s1 ->
                                Good secondCommitted (secondToChoice second) s1

                            Bad secondCommitted secondX () ->
                                if secondCommitted then
                                    Bad secondCommitted secondX ()

                                else
                                    Bad False (Append firstX secondX) ()
        )


orSucceed : Parser x a -> a -> Parser x a
orSucceed (Parser attemptFirst) secondRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted _ ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        Good False secondRes s
        )


oneOf2OrSucceed : Parser x a -> Parser x a -> a -> Parser x a
oneOf2OrSucceed (Parser attemptFirst) (Parser attemptSecond) thirdRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted _ ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted _ ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    Good False thirdRes s
        )


oneOf : x -> List (Parser x a) -> Parser x a
oneOf problemOnEmptyPossibilityList parsers =
    case parsers of
        [] ->
            Parser (\s -> Bad False (fromState s problemOnEmptyPossibilityList) ())

        (Parser parse) :: remainingParsers ->
            Parser
                (\s ->
                    case parse s of
                        (Good _ _ _) as step ->
                            step

                        (Bad committed x ()) as step ->
                            if committed then
                                step

                            else
                                oneOfHelp s x remainingParsers
                )


oneOfHelp : State -> RopeFilled (DeadEnd x) -> List (Parser x a) -> PStep x a
oneOfHelp s0 deadEnds parsers =
    case parsers of
        [] ->
            Bad False deadEnds ()

        (Parser parse) :: remainingParsers ->
            case parse s0 of
                (Good _ _ _) as step ->
                    step

                (Bad committed x ()) as step ->
                    if committed then
                        step

                    else
                        oneOfHelp s0 (Append deadEnds x) remainingParsers


{-| Decide what steps to take next in your `loop`.

If you are `Done`, you give the result of the whole `loop`. If you decide to
`Loop` around again, you give a new state to work from. Maybe you need to add
an item to a list? Or maybe you need to track some information about what you
just saw?

**Note:** It may be helpful to learn about [finite-state machines][fsm] to get
a broader intuition about using `state`. I.e. You may want to create a `type`
that describes four possible states, and then use `Loop` to transition between
them as you consume characters.

[fsm]: https://en.wikipedia.org/wiki/Finite-state_machine

-}
type Step state a
    = Loop state
    | Done a


{-| A parser that can loop indefinitely. This can be helpful when parsing
repeated structures, like a bunch of statements:


    statements : Parser (List Stmt)
    statements =
        loop [] statementsHelp

    statementsHelp : List Stmt -> Parser (Step (List Stmt) (List Stmt))
    statementsHelp revStmts =
        oneOf
            [ succeed (\stmt -> Loop (stmt :: revStmts))
                |= statement
                |> ParserFast.ignore spaces
                |> ParserFast.ignore symbol ";"
                |> ParserFast.ignore spaces
            , succeed ()
                |> map (\_ -> Done (List.reverse revStmts))
            ]

    -- statement : Parser Stmt

Notice that the statements are tracked in reverse as we `Loop`, and we reorder
them only once we are `Done`. This is a very common pattern with `loop`!

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example.

**IMPORTANT NOTE:** Parsers like `chompWhile Char.isAlpha` can
succeed without consuming any characters. So in some cases you may want to e.g.
use an [`ifFollowedByWhile`](#ifFollowedByWhile) to ensure that each step actually consumed characters.
Otherwise you could end up in an infinite loop!

**Note:** Anything you can write with `loop`, you can also write as a parser
that chomps some characters `andThen` calls itself with new arguments. The
problem with calling `andThen` recursively is that it grows the stack, so you
cannot do it indefinitely. So `loop` is important because enables tail-call
elimination, allowing you to parse however many repeats you want.

-}
loop : state -> Parser x extension -> (extension -> state -> Step state a) -> Parser x a
loop state element reduce =
    Parser
        (\s -> loopHelp False state element reduce s)


loopHelp : Bool -> state -> Parser x extension -> (extension -> state -> Step state a) -> State -> PStep x a
loopHelp committedSoFar state ((Parser parseElement) as element) reduce s0 =
    case parseElement s0 of
        Good elementCommitted step s1 ->
            case reduce step state of
                Loop newState ->
                    loopHelp (committedSoFar || elementCommitted) newState element reduce s1

                Done result ->
                    Good (committedSoFar || elementCommitted) result s1

        Bad elementCommitted x () ->
            Bad (committedSoFar || elementCommitted) x ()


loopWhileSucceeds : Parser x element -> folded -> (element -> folded -> folded) -> (folded -> res) -> Parser x res
loopWhileSucceeds element initialFolded reduce foldedToRes =
    Parser
        (\s -> loopWhileSucceedsHelp False element initialFolded reduce foldedToRes s)


loopWhileSucceedsHelp : Bool -> Parser x element -> folded -> (element -> folded -> folded) -> (folded -> res) -> State -> PStep x res
loopWhileSucceedsHelp committedSoFar ((Parser parseElement) as element) soFar reduce foldedToRes s0 =
    case parseElement s0 of
        Good elementCommitted elementResult s1 ->
            loopWhileSucceedsHelp (committedSoFar || elementCommitted)
                element
                (soFar |> reduce elementResult)
                reduce
                foldedToRes
                s1

        Bad elementCommitted x () ->
            if elementCommitted then
                Bad True x ()

            else
                Good committedSoFar (foldedToRes soFar) s0


loopUntil : Parser x () -> Parser x element -> folded -> (element -> folded -> folded) -> (folded -> res) -> Parser x res
loopUntil endParser element initialFolded reduce foldedToRes =
    Parser
        (\s -> loopUntilHelp False endParser element initialFolded reduce foldedToRes s)


loopUntilHelp : Bool -> Parser x () -> Parser x element -> folded -> (element -> folded -> folded) -> (folded -> res) -> State -> PStep x res
loopUntilHelp committedSoFar ((Parser parseEnd) as endParser) ((Parser parseElement) as element) soFar reduce foldedToRes s0 =
    case parseEnd s0 of
        Good endCommitted () s1 ->
            Good (committedSoFar || endCommitted) (foldedToRes soFar) s1

        Bad endCommitted endX () ->
            if endCommitted then
                Bad True endX ()

            else
                case parseElement s0 of
                    Good elementCommitted elementResult s1 ->
                        loopUntilHelp (committedSoFar || elementCommitted)
                            endParser
                            element
                            (soFar |> reduce elementResult)
                            reduce
                            foldedToRes
                            s1

                    Bad elementCommitted x () ->
                        Bad (committedSoFar || elementCommitted) x ()


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters
-}
keyword : String -> x -> res -> Parser x res
keyword kwd expecting res =
    let
        kwdLength : Int
        kwdLength =
            String.length kwd

        kwdChars =
            String.toList kwd

        justLastKwdChar =
            case List.reverse kwdChars of
                [] ->
                    -- TODO error? make impossible?
                    Nothing

                last :: _ ->
                    Just last
    in
    Parser
        (\s ->
            case startEquals kwdChars s.remaining of
                Just newRemaining ->
                    if isNextCharAlphaNumOrUnderscore newRemaining then
                        Bad False (fromState s expecting) ()

                    else
                        Good True
                            res
                            { previousChar = justLastKwdChar
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + kwdLength
                            }

                Nothing ->
                    Bad False (fromState s expecting) ()
        )


startEquals : List a -> List a -> Maybe (List a)
startEquals startToCheckFor elements =
    case startToCheckFor of
        [] ->
            Just elements

        startToCheckForHead :: startToCheckForTail ->
            case elements of
                [] ->
                    Nothing

                head :: tail ->
                    if head == startToCheckForHead then
                        startEquals startToCheckForTail tail

                    else
                        Nothing


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters
-}
keywordFollowedBy : String -> x -> Parser x next -> Parser x next
keywordFollowedBy kwd expecting (Parser parseNext) =
    let
        kwdLength : Int
        kwdLength =
            String.length kwd

        kwdChars =
            String.toList kwd

        justLastKwdChar =
            case List.reverse kwdChars of
                [] ->
                    -- TODO error? make impossible?
                    Nothing

                last :: _ ->
                    Just last
    in
    Parser
        (\s ->
            case startEquals kwdChars s.remaining of
                Just newRemaining ->
                    if isNextCharAlphaNumOrUnderscore newRemaining then
                        Bad False (fromState s expecting) ()

                    else
                        parseNext
                            { previousChar = justLastKwdChar
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + kwdLength
                            }
                            |> pStepCommit

                Nothing ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters
-}
symbol : String -> x -> res -> Parser x res
symbol str expecting res =
    let
        strLength : Int
        strLength =
            String.length str

        strChars =
            String.toList str

        justLastStrChar =
            case List.reverse strChars of
                [] ->
                    -- TODO error? make impossible?
                    Nothing

                last :: _ ->
                    Just last
    in
    Parser
        (\s ->
            case startEquals strChars s.remaining of
                Just newRemaining ->
                    Good True
                        res
                        { previousChar = justLastStrChar
                        , remaining = newRemaining
                        , indent = s.indent
                        , row = s.row
                        , col = s.col + strLength
                        }

                Nothing ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given Chars do not contain \\n
or 2-part UTF-16 characters
-}
symbol2 : Char -> Char -> x -> res -> Parser x res
symbol2 symbolChar0 symbolChar1 expecting res =
    Parser
        (\s ->
            case s.remaining of
                char0 :: char1 :: newRemaining ->
                    if char0 == symbolChar0 && char1 == symbolChar1 then
                        Good True
                            res
                            { previousChar = Just char1
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + 2
                            }

                    else
                        Bad False (fromState s expecting) ()

                _ ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given Chars do not contain \\n
or 2-part UTF-16 characters
-}
symbol3 : Char -> Char -> Char -> x -> res -> Parser x res
symbol3 symbolChar0 symbolChar1 symbolChar2 expecting res =
    Parser
        (\s ->
            case s.remaining of
                char0 :: char1 :: char2 :: newRemaining ->
                    if char0 == symbolChar0 && char1 == symbolChar1 && char2 == symbolChar2 then
                        Good True
                            res
                            { previousChar = Just char2
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + 3
                            }

                    else
                        Bad False (fromState s expecting) ()

                _ ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given Char does not contain \\n
or 2-part UTF-16 characters
-}
symbol1 : Char -> x -> res -> Parser x res
symbol1 symbolChar expecting res =
    Parser
        (\s ->
            case s.remaining of
                head :: newRemaining ->
                    if head == symbolChar then
                        Good True
                            res
                            { previousChar = Just head
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + 1
                            }

                    else
                        Bad False (fromState s expecting) ()

                _ ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given Char does not contain \\n
or 2-part UTF-16 characters
-}
symbol1Backtrackable : Char -> x -> res -> Parser x res
symbol1Backtrackable symbolChar expecting res =
    Parser
        (\s ->
            case s.remaining of
                head :: newRemaining ->
                    if head == symbolChar then
                        Good False
                            res
                            { previousChar = Just head
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + 1
                            }

                    else
                        Bad False (fromState s expecting) ()

                _ ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given Char does not contain \\n
or 2-part UTF-16 characters
-}
symbol1FollowedBy : Char -> x -> Parser x next -> Parser x next
symbol1FollowedBy symbolChar expecting (Parser parseNext) =
    Parser
        (\s ->
            case s.remaining of
                head :: newRemaining ->
                    if head == symbolChar then
                        parseNext
                            { previousChar = Just head
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + 1
                            }
                            |> pStepCommit

                    else
                        Bad False (fromState s expecting) ()

                _ ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given Chars do not contain \\n
or 2-part UTF-16 characters
-}
symbol2FollowedBy : Char -> Char -> x -> Parser x next -> Parser x next
symbol2FollowedBy symbolChar0 symbolChar1 expecting (Parser parseNext) =
    Parser
        (\s ->
            case s.remaining of
                char0 :: char1 :: newRemaining ->
                    if char0 == symbolChar0 && char1 == symbolChar1 then
                        parseNext
                            { previousChar = Just char1
                            , remaining = newRemaining
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + 2
                            }
                            |> pStepCommit

                    else
                        Bad False (fromState s expecting) ()

                _ ->
                    Bad False (fromState s expecting) ()
        )


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters
-}
symbolFollowedBy : String -> x -> Parser x next -> Parser x next
symbolFollowedBy str expecting (Parser parseNext) =
    let
        strLength : Int
        strLength =
            String.length str

        strChars =
            String.toList str

        justLastStrChar =
            case List.reverse strChars of
                [] ->
                    -- TODO error? make impossible?
                    Nothing

                last :: _ ->
                    Just last
    in
    Parser
        (\s ->
            case startEquals strChars s.remaining of
                Just newRemaining ->
                    parseNext
                        { previousChar = justLastStrChar
                        , remaining = newRemaining
                        , indent = s.indent
                        , row = s.row
                        , col = s.col + strLength
                        }
                        |> pStepCommit

                Nothing ->
                    Bad False (fromState s expecting) ()
        )


pStepCommit : PStep x a -> PStep x a
pStepCommit pStep =
    case pStep of
        Good _ a state ->
            Good True a state

        Bad _ errors () ->
            Bad True errors ()


number :
    { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }
    -> Parser x a
number c =
    let
        parserAdvancedNumberAndStringLength : Parser.Advanced.Parser c x { length : Int, number : a }
        parserAdvancedNumberAndStringLength =
            Parser.Advanced.map (\n -> \endOffset -> { length = endOffset, number = n })
                (Parser.Advanced.number c)
                |= Parser.Advanced.getOffset
                |. Parser.Advanced.end c.invalid
    in
    Parser
        (\state ->
            --case state.remaining of
            --    '0' :: 'x' :: remainingAfterHexMarker ->
            --        case c.hex of
            --            Err _ ->
            --                let
            --                    relevant : { result : String, state : State }
            --                    relevant =
            --                        chompWhileHelp "" (\ch -> Char.isDigit ch || ch == 'a' || ch == 'b' || ch == 'c' || ch == 'd' || ch == 'e' || ch == 'f') (Just 'x') state.row state.col remainingAfterHexMarker state.indent
            --                in
            --                case relevant.result of
            --                    "" ->
            --                        Bad False (fromState state c.invalid) ()
            --
            --                    _ ->
            --
            --                                Good False (hexToRes (Hex.fromString relevant.result)) relevant.state
            --    _ ->
            let
                relevant : { result : String, state : State }
                relevant =
                    chompWhileHelp "" (\ch -> Char.isAlphaNum ch || ch == '.') state.previousChar state.row state.col state.remaining state.indent
            in
            case relevant.result of
                "" ->
                    Bad False (fromState state c.invalid) ()

                _ ->
                    case Parser.Advanced.run parserAdvancedNumberAndStringLength relevant.result of
                        Ok result ->
                            Good False result.number relevant.state

                        Err _ ->
                            Bad False (fromState state c.invalid) ()
        )


end : x -> Parser x ()
end x =
    Parser
        (\s ->
            case s.remaining of
                _ :: _ ->
                    Bad False (fromState s x) ()

                [] ->
                    Good False () s
        )


anyChar : x -> Parser x Char
anyChar expecting =
    Parser
        (\s ->
            case s.remaining of
                char :: newRemaining ->
                    case char of
                        '\n' ->
                            Good True
                                '\n'
                                { previousChar = Just '\n'
                                , remaining = newRemaining
                                , indent = s.indent
                                , row = s.row + 1
                                , col = 1
                                }

                        _ ->
                            Good True
                                char
                                { previousChar = Just char
                                , remaining = newRemaining
                                , indent = s.indent
                                , row = s.row
                                , col = s.col + 1
                                }

                [] ->
                    Bad False (fromState s expecting) ()
        )


chompWhileWhitespaceFollowedBy : Parser x next -> Parser x next
chompWhileWhitespaceFollowedBy (Parser parseNext) =
    Parser
        (\s0 ->
            let
                s1 : State
                s1 =
                    chompWhileWhitespaceHelp s0.previousChar s0.row s0.col s0.remaining s0.indent
            in
            if s1.row == s0.row && s1.col == s0.col then
                parseNext s1

            else
                parseNext s1
                    |> pStepCommit
        )


chompWhileWhitespaceHelp : Maybe Char -> Int -> Int -> List Char -> Int -> State
chompWhileWhitespaceHelp previousChar row col remaining indent =
    case remaining of
        head :: tail ->
            case head of
                ' ' ->
                    chompWhileWhitespaceHelp (Just head) row (col + 1) tail indent

                '\n' ->
                    chompWhileWhitespaceHelp (Just head) (row + 1) 1 tail indent

                '\u{000D}' ->
                    chompWhileWhitespaceHelp (Just head) row (col + 1) tail indent

                -- empty or non-whitespace
                _ ->
                    { previousChar = previousChar, remaining = remaining, indent = indent, row = row, col = col }

        [] ->
            { previousChar = previousChar, remaining = remaining, indent = indent, row = row, col = col }


while : (Char -> Bool) -> Parser x String
while isGood =
    Parser
        (\s0 ->
            let
                s1 : { result : String, state : State }
                s1 =
                    chompWhileHelp "" isGood s0.previousChar s0.row s0.col s0.remaining s0.indent
            in
            Good (s1.result /= "")
                s1.result
                s1.state
        )


whileMap : (Char -> Bool) -> (String -> res) -> Parser x res
whileMap isGood chompedStringToRes =
    Parser
        (\s0 ->
            let
                s1 : { result : String, state : State }
                s1 =
                    chompWhileHelp "" isGood s0.previousChar s0.row s0.col s0.remaining s0.indent
            in
            Good (s1.result /= "")
                (chompedStringToRes s1.result)
                s1.state
        )


chompWhileHelp : String -> (Char -> Bool) -> Maybe Char -> Int -> Int -> List Char -> Int -> { result : String, state : State }
chompWhileHelp resultSoFar isGood previousChar row col remaining indent =
    case remaining of
        head :: tail ->
            if isGood head then
                case head of
                    '\n' ->
                        chompWhileHelp (resultSoFar ++ String.fromChar head) isGood (Just head) (row + 1) 1 tail indent

                    _ ->
                        chompWhileHelp (resultSoFar ++ String.fromChar head) isGood (Just head) row (col + 1) tail indent

            else
                { result = resultSoFar
                , state =
                    { previousChar = previousChar
                    , remaining = remaining
                    , indent = indent
                    , row = row
                    , col = col
                    }
                }

        [] ->
            { result = resultSoFar
            , state =
                { previousChar = previousChar
                , remaining = remaining
                , indent = indent
                , row = row
                , col = col
                }
            }


ifFollowedByWhileExcept :
    (Char -> Bool)
    -> (Char -> Bool)
    -> Set.Set String
    -> x
    -> Parser x String
ifFollowedByWhileExcept firstIsOkay afterFirstIsOkay exceptionSet expecting =
    Parser
        (\s ->
            case s.remaining of
                firstChar :: remainingAfterFirstChar ->
                    if firstIsOkay firstChar then
                        case firstChar of
                            '\n' ->
                                let
                                    s1 : { result : String, state : State }
                                    s1 =
                                        chompWhileHelp "" afterFirstIsOkay (Just firstChar) (s.row + 1) 1 remainingAfterFirstChar s.indent

                                    name : String
                                    name =
                                        String.cons firstChar s1.result
                                in
                                if Set.member name exceptionSet then
                                    Bad False (fromState s expecting) ()

                                else
                                    Good True name s1.state

                            _ ->
                                let
                                    s1 : { result : String, state : State }
                                    s1 =
                                        chompWhileHelp "" afterFirstIsOkay (Just firstChar) s.row (s.col + 1) remainingAfterFirstChar s.indent

                                    name : String
                                    name =
                                        String.cons firstChar s1.result
                                in
                                if Set.member name exceptionSet then
                                    Bad False (fromState s expecting) ()

                                else
                                    Good True name s1.state

                    else
                        Bad False (fromState s expecting) ()

                [] ->
                    Bad False (fromState s expecting) ()
        )


ifFollowedByWhile :
    (Char -> Bool)
    -> x
    -> (Char -> Bool)
    -> Parser x String
ifFollowedByWhile firstIsOkay problemOnFirstNotOkay afterFirstIsOkay =
    Parser
        (\s ->
            case s.remaining of
                firstChar :: remainingAfterFirstChar ->
                    if firstIsOkay firstChar then
                        case firstChar of
                            '\n' ->
                                let
                                    s1 : { result : String, state : State }
                                    s1 =
                                        chompWhileHelp "" afterFirstIsOkay (Just firstChar) (s.row + 1) 1 remainingAfterFirstChar s.indent
                                in
                                Good True (String.cons firstChar s1.result) s1.state

                            _ ->
                                let
                                    s1 : { result : String, state : State }
                                    s1 =
                                        chompWhileHelp "" afterFirstIsOkay (Just firstChar) s.row (s.col + 1) remainingAfterFirstChar s.indent
                                in
                                Good True (String.cons firstChar s1.result) s1.state

                    else
                        Bad False (fromState s problemOnFirstNotOkay) ()

                [] ->
                    Bad False (fromState s problemOnFirstNotOkay) ()
        )


anyCharFollowedByWhileMap :
    (String -> res)
    -> x
    -> (Char -> Bool)
    -> Parser x res
anyCharFollowedByWhileMap chompedStringToRes expectingAnyChar afterFirstIsOkay =
    Parser
        (\s ->
            case s.remaining of
                firstChar :: remainingAfterFirstChar ->
                    case firstChar of
                        '\n' ->
                            let
                                s1 : { result : String, state : State }
                                s1 =
                                    chompWhileHelp "" afterFirstIsOkay (Just firstChar) (s.row + 1) 1 remainingAfterFirstChar s.indent
                            in
                            Good True (chompedStringToRes (String.cons firstChar s1.result)) s1.state

                        _ ->
                            let
                                s1 : { result : String, state : State }
                                s1 =
                                    chompWhileHelp "" afterFirstIsOkay (Just firstChar) s.row (s.col + 1) remainingAfterFirstChar s.indent
                            in
                            Good True (chompedStringToRes (String.cons firstChar s1.result)) s1.state

                [] ->
                    Bad False (fromState s expectingAnyChar) ()
        )


nestableMultiComment : ( Char, String ) -> x -> ( Char, String ) -> x -> Parser x String
nestableMultiComment ( openChar, openTail ) expectingOpen ( closeChar, closeTail ) expectingClose =
    let
        open : String
        open =
            String.cons openChar openTail

        close : String
        close =
            String.cons closeChar closeTail

        isNotRelevant : Char -> Bool
        isNotRelevant char =
            char /= openChar && char /= closeChar
    in
    map2
        (\afterOpen contentAfterAfterOpen ->
            open ++ afterOpen ++ contentAfterAfterOpen ++ close
        )
        (symbolFollowedBy open
            expectingOpen
            (while isNotRelevant)
        )
        (oneOf2
            (symbol close expectingClose "")
            (loop
                ( "", 1 )
                (oneOf3
                    (symbol close expectingClose ( close, -1 ))
                    (symbol open expectingOpen ( open, 1 ))
                    (anyCharFollowedByWhileMap (\chomped -> ( chomped, 0 ))
                        expectingClose
                        isNotRelevant
                    )
                )
                (\( toAppend, nestingChange ) ( soFarContent, soFarNesting ) ->
                    let
                        newNesting : Int
                        newNesting =
                            soFarNesting + nestingChange
                    in
                    if newNesting == 0 then
                        Done soFarContent

                    else
                        Loop ( soFarContent ++ toAppend ++ "", newNesting )
                )
            )
        )


withIndent : Int -> Parser x a -> Parser x a
withIndent newIndent (Parser parse) =
    Parser
        (\s0 ->
            case parse (changeIndent newIndent s0) of
                Good committed a s1 ->
                    Good committed a (changeIndent s0.indent s1)

                bad ->
                    bad
        )


withIndentSetToColumn : Parser x a -> Parser x a
withIndentSetToColumn (Parser parse) =
    Parser
        (\s0 ->
            case parse (changeIndent s0.col s0) of
                Good committed a s1 ->
                    Good committed a (changeIndent s0.indent s1)

                bad ->
                    bad
        )


changeIndent : Int -> State -> State
changeIndent newIndent s =
    { remaining = s.remaining
    , previousChar = s.previousChar
    , indent = newIndent
    , row = s.row
    , col = s.col
    }


mapWithStartPosition :
    ({ row : Int, column : Int } -> a -> b)
    -> Parser x a
    -> Parser x b
mapWithStartPosition combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good committed a s1 ->
                    Good committed (combineStartAndResult { row = s0.row, column = s0.col } a) s1

                Bad committed x () ->
                    Bad committed x ()
        )


mapWithEndPosition :
    (a -> { row : Int, column : Int } -> b)
    -> Parser x a
    -> Parser x b
mapWithEndPosition combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good committed a s1 ->
                    Good committed (combineStartAndResult a { row = s1.row, column = s1.col }) s1

                Bad committed x () ->
                    Bad committed x ()
        )


mapWithStartAndEndPosition :
    ({ row : Int, column : Int } -> a -> { row : Int, column : Int } -> b)
    -> Parser x a
    -> Parser x b
mapWithStartAndEndPosition combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good committed a s1 ->
                    Good committed (combineStartAndResult { row = s0.row, column = s0.col } a { row = s1.row, column = s1.col }) s1

                Bad committed x () ->
                    Bad committed x ()
        )


isNextCharAlphaNumOrUnderscore : List Char -> Bool
isNextCharAlphaNumOrUnderscore remaining =
    case remaining of
        head :: _ ->
            Char.Extra.isAlphaNumFast head || head == '_'

        [] ->
            False
