module Elm.Writer exposing (write, writeFile, writePattern, writeExpression, writeTypeAnnotation, writeDeclaration)

{-| Write a file to a string.

@docs write, writeFile, writePattern, writeExpression, writeTypeAnnotation, writeDeclaration

-}

import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Documentation exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.ModuleName exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Port exposing (Port)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAlias exposing (..)
import Elm.Syntax.TypeAnnotation exposing (..)
import Hex
import List.Extra
import StructuredWriter as Writer exposing (..)


{-| Transform a writer to a string
-}
write : Writer -> String
write =
    Writer.write


{-| Write a file
-}
writeFile : File -> Writer
writeFile file =
    breaked
        [ writeModule <| Node.value file.moduleDefinition
        , breaked (List.map (Node.value >> writeImport) file.imports)
        , breaked (List.map writeDeclaration file.declarations)
        ]


writeModule : Module -> Writer
writeModule m =
    case m of
        NormalModule defaultModuleData ->
            writeDefaultModuleData defaultModuleData

        PortModule defaultModuleData ->
            spaced
                [ string "port"
                , writeDefaultModuleData defaultModuleData
                ]

        EffectModule effectModuleData ->
            writeEffectModuleData effectModuleData


writeDefaultModuleData : DefaultModuleData -> Writer
writeDefaultModuleData { moduleName, exposingList } =
    spaced
        [ string "module"
        , writeModuleName <| Node.value moduleName
        , writeExposureExpose <| Node.value exposingList
        ]


writeEffectModuleData : EffectModuleData -> Writer
writeEffectModuleData { moduleName, exposingList, command, subscription } =
    spaced
        [ string "effect"
        , string "module"
        , writeModuleName <| Node.value moduleName
        , writeWhere ( command, subscription )
        , writeExposureExpose <| Node.value exposingList
        ]


writeWhere : ( Maybe (Node String), Maybe (Node String) ) -> Writer
writeWhere input =
    case input of
        ( Nothing, Nothing ) ->
            epsilon

        ( Just x, Nothing ) ->
            spaced
                [ string "where { command ="
                , string <| Node.value x
                , string "}"
                ]

        ( Nothing, Just x ) ->
            spaced
                [ string "where { subscription ="
                , string <| Node.value x
                , string "}"
                ]

        ( Just x, Just y ) ->
            spaced
                [ string "where { command ="
                , string <| Node.value x
                , string ", subscription ="
                , string <| Node.value y
                , string "}"
                ]


writeModuleName : ModuleName -> Writer
writeModuleName moduleName =
    string (String.join "." moduleName)


writeExposureExpose : Exposing -> Writer
writeExposureExpose x =
    case x of
        All _ ->
            string "exposing (..)"

        Explicit head rest ->
            let
                exposeList : List (Node TopLevelExpose)
                exposeList =
                    head :: rest

                diffLines : Bool
                diffLines =
                    List.map Node.range exposeList
                        |> startOnDifferentLines
            in
            spaced
                [ string "exposing"
                , parensComma diffLines (List.map writeExpose exposeList)
                ]


writeExpose : Node TopLevelExpose -> Writer
writeExpose (Node _ exp) =
    case exp of
        InfixExpose x ->
            string ("(" ++ x ++ ")")

        FunctionExpose f ->
            string f

        TypeOrAliasExpose t ->
            string t

        TypeExpose { name, open } ->
            case open of
                Just _ ->
                    spaced
                        [ string name
                        , string "(..)"
                        ]

                Nothing ->
                    string name


startOnDifferentLines : List Range -> Bool
startOnDifferentLines xs =
    List.length (List.Extra.unique (List.map (.start >> .row) xs)) > 1


writeImport : Import -> Writer
writeImport { moduleName, moduleAlias, exposingList } =
    spaced
        [ string "import"
        , writeModuleName <| Node.value moduleName
        , maybe (Maybe.map (Node.value >> writeModuleName >> (\x -> spaced [ string "as", x ])) moduleAlias)
        , maybe (Maybe.map (Node.value >> writeExposureExpose) exposingList)
        ]


writeLetDeclaration : Node LetDeclaration -> Writer
writeLetDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        LetFunction function ->
            writeFunction function

        LetDestructuring pattern expression ->
            writeDestructuring pattern expression


{-| Write a declaration
-}
writeDeclaration : Node Declaration -> Writer
writeDeclaration (Node _ decl) =
    case decl of
        FunctionDeclaration function ->
            writeFunction function

        AliasDeclaration typeAlias ->
            writeTypeAlias typeAlias

        CustomTypeDeclaration type_ ->
            writeType type_

        PortDeclaration p ->
            writePortDeclaration p

        InfixDeclaration i ->
            writeInfix i


writeFunction : Function -> Writer
writeFunction { documentation, signature, declaration } =
    breaked
        [ maybe (Maybe.map writeDocumentation documentation)
        , maybe (Maybe.map (Node.value >> writeSignature) signature)
        , writeFunctionImplementation <| Node.value declaration
        ]


writeFunctionImplementation : FunctionImplementation -> Writer
writeFunctionImplementation declaration =
    breaked
        [ spaced
            [ string <| Node.value declaration.name
            , spaced (List.map writeDestructurePattern declaration.arguments)
            , string "="
            ]
        , indent 4 (writeExpression declaration.expression)
        ]


writeSignature : Signature -> Writer
writeSignature signature =
    spaced
        [ string <| Node.value signature.name
        , string ":"
        , writeTypeAnnotation signature.typeAnnotation
        ]


writeDocumentation : Node Documentation -> Writer
writeDocumentation =
    Node.value >> string


writeTypeAlias : TypeAlias -> Writer
writeTypeAlias typeAlias =
    breaked
        [ spaced
            [ string "type alias"
            , string <| Node.value typeAlias.name
            , spaced (List.map (Node.value >> string) typeAlias.generics)
            , string "="
            ]
        , indent 4 (writeTypeAnnotation typeAlias.typeAnnotation)
        ]


writeType : Type -> Writer
writeType type_ =
    breaked
        [ spaced
            [ string "type"
            , string <| Node.value type_.name
            , spaced (List.map (Node.value >> string) type_.generics)
            ]
        , let
            constructors : List (Node ValueConstructor)
            constructors =
                type_.firstConstructor :: type_.restOfConstructors

            diffLines : Bool
            diffLines =
                List.map Node.range constructors
                    |> startOnDifferentLines
          in
          indent 4
            (sepBy ( "= ", " | ", "" )
                diffLines
                (List.map (Node.value >> writeValueConstructor) constructors)
            )
        ]


writeValueConstructor : ValueConstructor -> Writer
writeValueConstructor { name, arguments } =
    spaced
        ((string <| Node.value name)
            :: List.map (wrapInSurroundingParentheses >> writeTypeAnnotation) arguments
        )


writePortDeclaration : Port -> Writer
writePortDeclaration { signature, documentation } =
    case documentation of
        Just doc ->
            breaked
                [ writeDocumentation doc
                , spaced [ string "port", writeSignature <| Node.value signature ]
                ]

        Nothing ->
            spaced [ string "port", writeSignature <| Node.value signature ]


writeInfix : Infix -> Writer
writeInfix { direction, precedence, operator, function } =
    spaced
        [ string "infix"
        , case Node.value direction of
            Left ->
                string "left"

            Right ->
                string "right"

            Non ->
                string "non"
        , string (String.fromInt (Node.value precedence))
        , string (Node.value operator)
        , string "="
        , string (Node.value function)
        ]


writeDestructuring : Node DestructurePattern -> Node Expression -> Writer
writeDestructuring pattern expression =
    breaked
        [ spaced [ writeDestructurePattern pattern, string "=" ]
        , indent 4 (writeExpression expression)
        ]


{-| Write a type annotation
-}
writeTypeAnnotation : Node TypeAnnotation -> Writer
writeTypeAnnotation (Node _ typeAnnotation) =
    case typeAnnotation of
        Var s ->
            string s

        Elm.Syntax.TypeAnnotation.Type (Node _ ( moduleName, name )) args ->
            spaced
                ((string <| String.join "." (moduleName ++ [ name ]))
                    :: List.map (writeTypeAnnotation >> parensIfContainsSpaces) args
                )

        Tuple xs ->
            parensComma False (List.map writeTypeAnnotation xs)

        Record xs ->
            bracesComma False (List.map writeRecordField xs)

        GenericRecord name fields ->
            spaced
                [ string "{"
                , string <| Node.value name
                , string "|"
                , sepByComma False (List.map writeRecordField <| Node.value fields)
                , string "}"
                ]

        FunctionTypeAnnotation left right ->
            let
                addParensForSubTypeAnnotation : Node TypeAnnotation -> Writer
                addParensForSubTypeAnnotation type_ =
                    case type_ of
                        Node _ (FunctionTypeAnnotation _ _) ->
                            join [ string "(", writeTypeAnnotation type_, string ")" ]

                        _ ->
                            writeTypeAnnotation type_
            in
            spaced
                [ addParensForSubTypeAnnotation left
                , string "->"
                , addParensForSubTypeAnnotation right
                ]


writeRecordField : Node RecordField -> Writer
writeRecordField (Node _ ( name, ref )) =
    spaced
        [ string <| Node.value name
        , string ":"
        , writeTypeAnnotation ref
        ]


{-| Writer an expression
-}
writeExpression : Node Expression -> Writer
writeExpression (Node range inner) =
    let
        recurRangeHelper : Node Expression -> ( Range, Writer )
        recurRangeHelper (Node x y) =
            ( x, writeExpression (Node x y) )

        writeRecordSetter : RecordSetter -> ( Range, Writer )
        writeRecordSetter ( name, expr ) =
            ( Node.range expr
            , spaced [ string <| Node.value name, string "=", writeExpression expr ]
            )

        sepHelper : (Bool -> List Writer -> Writer) -> List ( Range, Writer ) -> Writer
        sepHelper f l =
            let
                diffLines : Bool
                diffLines =
                    List.map Tuple.first l
                        |> startOnDifferentLines
            in
            f diffLines (List.map Tuple.second l)
    in
    case inner of
        Application head xs ->
            case xs of
                [] ->
                    writeExpression head

                rest ->
                    spaced
                        [ writeExpression head
                        , sepHelper sepBySpace (List.map recurRangeHelper rest)
                        ]

        OperatorApplication x dir left right ->
            case dir of
                Left ->
                    sepHelper sepBySpace
                        [ ( Node.range left, writeExpression left )
                        , ( range, spaced [ string x, writeExpression right ] )
                        ]

                Right ->
                    sepHelper sepBySpace
                        [ ( Node.range left, spaced [ writeExpression left, string x ] )
                        , ( Node.range right, writeExpression right )
                        ]

                Non ->
                    sepHelper sepBySpace
                        [ ( Node.range left, spaced [ writeExpression left, string x ] )
                        , ( Node.range right, writeExpression right )
                        ]

        FunctionOrValue moduleName name ->
            case moduleName of
                [] ->
                    string name

                _ ->
                    join
                        [ writeModuleName <| moduleName
                        , string "."
                        , string <| name
                        ]

        If condition thenCase elseCase ->
            breaked
                [ spaced [ string "if", writeExpression condition, string "then" ]
                , indent 2 (writeExpression thenCase)
                , string "else"
                , indent 2 (writeExpression elseCase)
                ]

        PrefixOperator x ->
            string ("(" ++ x ++ ")")

        Operator x ->
            string x

        HexLiteral h ->
            join [ string "0x", string (Hex.toString h) ]

        IntegerLiteral i ->
            string (String.fromInt i)

        FloatLiteral f ->
            string (String.fromFloat f)

        Negation x ->
            append (string "-") (writeExpression x)

        StringLiteral SingleQuote s ->
            string ("\"" ++ s ++ "\"")

        StringLiteral TripleQuote s ->
            string ("\"\"\"" ++ s ++ "\"\"\"")

        CharLiteral c ->
            writeChar c

        TupleExpression t ->
            join [ string "(", sepHelper sepByComma (List.map recurRangeHelper t), string ")" ]

        LetExpression letBlock ->
            breaked
                [ string "let"
                , indent 2 (breaked (List.map writeLetDeclaration letBlock.declarations))
                , string "in"
                , indent 2 (writeExpression letBlock.expression)
                ]

        CaseExpression caseBlock ->
            let
                writeCaseBranch : ( Node Pattern, Node Expression ) -> Writer
                writeCaseBranch ( pattern, expression ) =
                    indent 2 <|
                        breaked
                            [ spaced [ writePattern pattern, string "->" ]
                            , indent 2 (writeExpression expression)
                            ]
            in
            breaked
                [ string ""
                , spaced [ string "case", writeExpression caseBlock.expression, string "of" ]
                , breaked (List.map writeCaseBranch (caseBlock.firstCase :: caseBlock.restOfCases))
                , string ""
                ]

        LambdaExpression lambda ->
            spaced
                [ join
                    [ string "\\"
                    , spaced (List.map writeDestructurePattern (lambda.firstArg :: lambda.restOfArgs))
                    ]
                , string "->"
                , writeExpression lambda.expression
                ]

        RecordExpr setters ->
            sepHelper bracesComma (List.map (Node.value >> writeRecordSetter) setters)

        ListLiteral xs ->
            sepHelper bracketsComma (List.map recurRangeHelper xs)

        RecordAccess expression accessor ->
            join [ writeExpression expression, string ".", string <| Node.value accessor ]

        RecordAccessFunction s ->
            if String.startsWith "." s then
                string s

            else
                join [ string ".", string s ]

        RecordUpdateExpression name firstUpdate updates ->
            spaced
                [ string "{"
                , string <| Node.value name
                , string "|"
                , sepHelper sepByComma (List.map (Node.value >> writeRecordSetter) (firstUpdate :: updates))
                , string "}"
                ]

        GLSLExpression s ->
            join
                [ string "[glsl|"
                , string s
                , string "|]"
                ]


escapeString : String -> String
escapeString =
    String.replace "\"" "\\\""


writeChar : Char -> Writer
writeChar c =
    let
        escape : String
        escape =
            if c == '\t' || c == '\'' || c == '\\' then
                "\\"

            else
                ""
    in
    string ("'" ++ escape ++ String.fromChar c ++ "'")


{-| Write a pattern
-}
writePattern : Node Pattern -> Writer
writePattern (Node _ p) =
    case p of
        AllPattern ->
            string "_"

        UnitPattern ->
            string "()"

        CharPattern c ->
            writeChar c

        StringPattern s ->
            string ("\"" ++ escapeString s ++ "\"")

        HexPattern h ->
            join [ string "0x", string (Hex.toString h) ]

        IntPattern i ->
            string (String.fromInt i)

        TuplePattern inner ->
            parensComma False (List.map writePattern inner)

        RecordPattern inner ->
            bracesComma False (List.map (Node.value >> string) inner)

        UnConsPattern left right ->
            spaced [ writePattern left, string "::", writePattern right ]

        ListPattern inner ->
            bracketsComma False (List.map writePattern inner)

        VarPattern var ->
            string var

        NamedPattern qnr others ->
            spaced
                [ writeQualifiedNameRef qnr
                , spaced (List.map writePattern others)
                ]

        AsPattern innerPattern asName ->
            spaced [ writePattern innerPattern, string "as", string <| Node.value asName ]

        ParenthesizedPattern innerPattern ->
            spaced [ string "(", writePattern innerPattern, string ")" ]


writeDestructurePattern : Node DestructurePattern -> Writer
writeDestructurePattern (Node _ p) =
    case p of
        AllPattern_ ->
            string "_"

        UnitPattern_ ->
            string "()"

        TuplePattern_ inner ->
            parensComma False (List.map writeDestructurePattern inner)

        RecordPattern_ inner ->
            bracesComma False (List.map (Node.value >> string) inner)

        VarPattern_ var ->
            string var

        NamedPattern_ qnr others ->
            spaced
                [ writeQualifiedNameRef qnr
                , spaced (List.map writeDestructurePattern others)
                ]

        AsPattern_ innerPattern asName ->
            spaced [ writeDestructurePattern innerPattern, string "as", string <| Node.value asName ]

        ParenthesizedPattern_ innerPattern ->
            spaced [ string "(", writeDestructurePattern innerPattern, string ")" ]


writeQualifiedNameRef : QualifiedNameRef -> Writer
writeQualifiedNameRef { moduleName, name } =
    case moduleName of
        [] ->
            string name

        _ ->
            join
                [ writeModuleName moduleName
                , string "."
                , string name
                ]



-- Helpers


parensIfContainsSpaces : Writer -> Writer
parensIfContainsSpaces w =
    if Writer.write w |> String.contains " " then
        join [ string "(", w, string ")" ]

    else
        w


wrapInSurroundingParentheses : Node TypeAnnotation -> Node TypeAnnotation
wrapInSurroundingParentheses node =
    let
        withParens : Node TypeAnnotation -> Node TypeAnnotation
        withParens n =
            Node.empty (Tuple [ n ])
    in
    case Node.value node of
        FunctionTypeAnnotation _ _ ->
            withParens node

        Elm.Syntax.TypeAnnotation.Type _ typeParameters ->
            case typeParameters of
                [] ->
                    node

                _ ->
                    withParens node

        _ ->
            node
