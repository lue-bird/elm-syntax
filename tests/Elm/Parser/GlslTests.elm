module Elm.Parser.GlslTests exposing (all)

import CustomParser
import Elm.Parser.Expression exposing (positivelyIndentedExpression)
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Test exposing (..)


all : Test
all =
    describe "GlslTests"
        [ test "case block" <|
            \() ->
                "[glsl| precision mediump float; |]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            (GLSLExpression " precision mediump float; ")
                        )
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst
        (CustomParser.withIndent 0
            positivelyIndentedExpression
        )
