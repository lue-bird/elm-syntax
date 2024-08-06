module Elm.WriterTests exposing (suite)

import CustomParser
import Elm.Parser.Expression exposing (positivelyIndentedExpression)
import Elm.Parser.ParserWithCommentsTestUtil exposing (parse)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Elm.Writer as Writer
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Writer"
        [ test "write file exposing all" <|
            \() ->
                { moduleDefinition =
                    Node empty <|
                        NormalModule
                            { moduleName = Node empty <| [ "A" ]
                            , exposingList = Node empty <| All empty
                            }
                , imports =
                    [ Node empty
                        { moduleName = Node empty <| [ "B" ]
                        , moduleAlias = Nothing
                        , exposingList = Nothing
                        }
                    , Node empty
                        { moduleName = Node empty <| [ "C" ]
                        , moduleAlias = Just (Node empty [ "D" ])
                        , exposingList = Just (Node empty <| All empty)
                        }
                    ]
                , declarations = []
                , comments = []
                }
                    |> Writer.writeFile
                    |> Writer.write
                    |> Expect.equal
                        ("module A exposing (..)\n"
                            ++ "import B  "
                            ++ "\n"
                            ++ "import C as D exposing (..)\n"
                        )
        , describe "Expression"
            [ test "write simple expression" <|
                \() ->
                    (Node empty <|
                        Application
                            [ Node empty <| FunctionOrValue [] "abc"
                            , Node empty <| UnitExpr
                            ]
                    )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "abc ()"
            , test "write qualified expression" <|
                \() ->
                    (Node empty <| FunctionOrValue [ "Foo", "Bar" ] "baz")
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "Foo.Bar.baz"
            , test "Expression.RecordAccessFunction should be parsed then written idempotently" <|
                \() ->
                    let
                        input : String
                        input =
                            "(.spaceEvenly Internal.Style.classes)"
                    in
                    parse input
                        (CustomParser.withIndent 0
                            positivelyIndentedExpression
                        )
                        |> Maybe.map Writer.writeExpression
                        |> Maybe.map Writer.write
                        |> Expect.equal
                            (Just input)
            , test "regression test for Expression.RecordAccessFunction being written without leading period" <|
                \() ->
                    (Node empty <|
                        Application
                            [ Node empty <| FunctionOrValue [ "List" ] "map"
                            , Node empty <| RecordAccessFunction "name"
                            , Node empty <| FunctionOrValue [] "people"
                            ]
                    )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "List.map .name people"
            ]
        , describe "Pattern"
            [ test "write string pattern" <|
                \() ->
                    StringPattern "test"
                        |> Node empty
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "\"test\""
            , test "write string pattern containing \"" <|
                \() ->
                    StringPattern "test\""
                        |> Node empty
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "\"test\\\"\""
            ]
        , describe "TypeAnnotation"
            [ test "write simple type" <|
                \() ->
                    Typed (Node empty <| ( [], "String" )) []
                        |> Node empty
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "String"
            , test "write qualified type" <|
                \() ->
                    (Node empty <|
                        Typed
                            (Node empty <| ( [ "Json", "Decode" ], "Decoder" ))
                            [ Node empty <| GenericType "a" ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "Json.Decode.Decoder a"
            , test "write type arguments that require parentheses" <|
                \() ->
                    (Node empty <|
                        Typed (Node empty ( [], "List" ))
                            [ Node empty <|
                                Typed (Node empty ( [], "Dict" ))
                                    [ Node empty <| Typed (Node empty ( [], "String" )) []
                                    , Node empty <| Typed (Node empty ( [], "Int" )) []
                                    ]
                            ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "List (Dict String Int)"
            , test "write type arguments that are functions" <|
                \() ->
                    (Node empty <|
                        FunctionTypeAnnotation
                            (Node empty <|
                                FunctionTypeAnnotation
                                    (Node empty <| GenericType "a")
                                    (Node empty <| GenericType "b")
                            )
                            (Node empty <| Typed (Node empty ( [], "Int" )) [])
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "(a -> b) -> Int"
            ]
        , describe "Declaration"
            [ test "write type declaration > simple constructors" <|
                \() ->
                    (Node empty <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node empty "Sample")
                                []
                                [ Node empty <| ValueConstructor (Node empty "Foo") []
                                , Node empty <| ValueConstructor (Node empty "Bar") []
                                ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo | Bar"
                            )
            , test "write type declaration > constructors with arguments" <|
                \() ->
                    let
                        listT : TypeAnnotation
                        listT =
                            Typed (Node empty ( [], "List" ))
                                [ Node empty <|
                                    Typed (Node empty ( [], "String" )) []
                                ]

                        stringT : TypeAnnotation
                        stringT =
                            Typed (Node empty ( [], "String" )) []
                    in
                    (Node empty <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node empty "Sample")
                                []
                                [ Node empty <| ValueConstructor (Node empty "Foo") [ Node empty listT, Node empty stringT ]
                                , Node empty <| ValueConstructor (Node empty "Bar") []
                                ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo (List String) String | Bar"
                            )
            , test "write type declaration > constructors with functions as arguments" <|
                \() ->
                    let
                        funcT : TypeAnnotation
                        funcT =
                            FunctionTypeAnnotation
                                (Node empty <| Typed (Node empty ( [], "String" )) [])
                                (Node empty <| Typed (Node empty ( [], "Int" )) [])

                        stringT : TypeAnnotation
                        stringT =
                            Typed (Node empty ( [], "String" )) []
                    in
                    (Node empty <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node empty "Sample")
                                []
                                [ Node empty <|
                                    ValueConstructor (Node empty "Foo")
                                        [ Node empty funcT
                                        , Node empty stringT
                                        ]
                                , Node empty <| ValueConstructor (Node empty "Bar") []
                                ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo (String -> Int) String | Bar"
                            )
            , test "write function with case expression using the right indentations" <|
                \() ->
                    let
                        body : Expression
                        body =
                            CaseExpression
                                (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                    [ ( Node empty <| IntPattern 1, Node empty <| FunctionOrValue [] "doSomething" )
                                    , ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                    ]
                                )

                        function : Declaration
                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty body)
                                    )
                                )
                    in
                    Node empty function
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("\n\nfunctionName  =\n"
                                ++ "    \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        doSomething\n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    "
                            )
            , test "regression test for incorrect indentation in case expression" <|
                \() ->
                    let
                        body : Expression
                        body =
                            LambdaExpression
                                { args = [ Node empty (VarPattern "myArgument") ]
                                , expression =
                                    Node empty <|
                                        CaseExpression
                                            (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                                [ ( Node empty <| IntPattern 1, Node empty <| FunctionOrValue [] "doSomething" )
                                                , ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                                ]
                                            )
                                }

                        function : Declaration
                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty body)
                                    )
                                )
                    in
                    Node empty function
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("\n\nfunctionName  =\n"
                                ++ "    \\myArgument -> \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        doSomething\n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    "
                            )
            , test "regression test for incorrect parenthesis placement in case expression" <|
                \() ->
                    let
                        body : Expression
                        body =
                            ParenthesizedExpression
                                (Node empty <|
                                    LambdaExpression
                                        { args = [ Node empty (VarPattern "myArgument") ]
                                        , expression =
                                            Node empty <|
                                                CaseExpression
                                                    (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                                        [ ( Node empty <| IntPattern 1, Node empty <| FunctionOrValue [] "doSomething" )
                                                        , ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                                        ]
                                                    )
                                        }
                                )

                        function : Declaration
                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty body)
                                    )
                                )
                    in
                    Node empty function
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("\n\nfunctionName  =\n"
                                ++ "    (\\myArgument -> \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        doSomething\n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    )"
                            )
            , test "regression test for char literals not being escaped" <|
                \() ->
                    ListExpr
                        [ Node empty (CharLiteral '\\')
                        , Node empty (CharLiteral '"')
                        , Node empty (CharLiteral '\'')
                        , Node empty (CharLiteral '\t')
                        , Node empty (CharLiteral '→')
                        , Node empty (CharLiteral '\u{00A0}')
                        ]
                        |> Node empty
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "['\\\\', '\"', '\\'', '\\\t', '→', '\u{00A0}']"
            , test "regression test for char pattern not being escaped" <|
                \() ->
                    ListPattern
                        [ Node empty (CharPattern '\\')
                        , Node empty (CharPattern '"')
                        , Node empty (CharPattern '\'')
                        , Node empty (CharPattern '\t')
                        , Node empty (CharPattern '→')
                        , Node empty (CharPattern '\u{00A0}')
                        ]
                        |> Node empty
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "['\\\\', '\"', '\\'', '\\\t', '→', '\u{00A0}']"
            , test "nested case expressions" <|
                \() ->
                    let
                        body : Node Expression -> Node Expression
                        body nested =
                            Node empty <|
                                CaseExpression
                                    (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                        [ ( Node empty <| IntPattern 1, nested )
                                        , ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                        ]
                                    )

                        function : Declaration
                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty
                                                (ParenthesizedExpression
                                                    (Node empty <|
                                                        LambdaExpression
                                                            { args = [ Node empty (VarPattern "myArgument") ]
                                                            , expression =
                                                                body (body (Node empty UnitExpr))
                                                            }
                                                    )
                                                )
                                            )
                                    )
                                )
                    in
                    Node empty function
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("\n\nfunctionName  =\n"
                                ++ "    (\\myArgument -> \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        \n"
                                ++ "        case someCase of\n"
                                ++ "          1 ->\n"
                                ++ "            ()\n"
                                ++ "          2 ->\n"
                                ++ "            doSomethingElse\n"
                                ++ "        \n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    )"
                            )
            ]
        , describe "Tuple"
            [ test "write tuple" <|
                \() ->
                    (Node empty <|
                        TupledExpression
                            [ Node empty <| Integer 1
                            , Node empty <| Integer 2
                            ]
                    )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "(1, 2)"
            ]
        ]
