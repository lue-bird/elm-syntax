module Elm.Parser.ParserWithCommentsTestUtil exposing (expectAst, expectAstWithComments, expectAstWithIndent1, expectInvalid, parse, parseWithState)

import Elm.Syntax.Node exposing (Node)
import Expect
import Parser
import ParserFast
import ParserWithComments exposing (WithComments)
import Rope


parseWithState : String -> ParserFast.Parser (WithComments a) -> Maybe ( List (Node String), a )
parseWithState s p =
    case ParserFast.run p s of
        Err _ ->
            Nothing

        Ok ( comments, syntax ) ->
            ( comments |> Rope.toList
            , syntax
            )
                |> Just


parse : String -> ParserFast.Parser (WithComments a) -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map Tuple.second


parseWithFailure : String -> ParserFast.Parser (WithComments a) -> Result (List Parser.DeadEnd) a
parseWithFailure s p =
    case ParserFast.run p s of
        Err deadEnds ->
            Err deadEnds

        Ok ( _, syntax ) ->
            syntax |> Ok


expectAstWithIndent1 : ParserFast.Parser (WithComments a) -> a -> String -> Expect.Expectation
expectAstWithIndent1 parser =
    \expected source ->
        case ParserFast.run parser source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok ( comments, syntax ) ->
                Expect.all
                    [ \() -> syntax |> Expect.equal expected
                    , \() ->
                        comments
                            |> Rope.toList
                            |> Expect.equalLists []
                            |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                    ]
                    ()


expectAst : ParserFast.Parser (WithComments a) -> a -> String -> Expect.Expectation
expectAst parser =
    \expected source ->
        case ParserFast.run parser source of
            Err deadEnds ->
                Expect.fail ("Expected the source to be parsed correctly:\n[ " ++ (List.map deadEndToString deadEnds |> String.join "\n, ") ++ "\n]")

            Ok ( comments, syntax ) ->
                Expect.all
                    [ \() -> syntax |> Expect.equal expected
                    , \() ->
                        comments
                            |> Rope.toList
                            |> Expect.equalLists []
                            |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                    ]
                    ()


deadEndToString : Parser.DeadEnd -> String
deadEndToString { row, col, problem } =
    "{ problem: " ++ Debug.toString problem ++ ", row = " ++ String.fromInt row ++ ", col = " ++ String.fromInt col ++ " }"


expectAstWithComments : ParserFast.Parser (WithComments a) -> { ast : a, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments parser =
    \expected source ->
        case ParserFast.run parser source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok ( comments, syntax ) ->
                Expect.all
                    [ \() -> syntax |> Expect.equal expected.ast
                    , \() -> comments |> Rope.toList |> Expect.equal expected.comments
                    ]
                    ()


expectInvalid : ParserFast.Parser (WithComments a) -> String -> Expect.Expectation
expectInvalid parser =
    \source ->
        case parseWithFailure source parser of
            Err _ ->
                Expect.pass

            Ok actual ->
                Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
