module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import CustomParser exposing (Parser)
import CustomParser.Extra
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import ParserWithComments exposing (WithComments)
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    CustomParser.map
        (\ta ->
            \afterTa ->
                case afterTa of
                    Nothing ->
                        ta

                    Just out ->
                        { comments = ta.comments |> Rope.prependTo out.comments
                        , syntax = Node.combine TypeAnnotation.FunctionTypeAnnotation ta.syntax out.syntax
                        }
        )
        (CustomParser.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
                    (\commentsBeforeArrow ->
                        \commentsAfterArrow ->
                            \typeAnnotationResult ->
                                Just
                                    { comments =
                                        commentsBeforeArrow
                                            |> Rope.prependTo commentsAfterArrow
                                            |> Rope.prependTo typeAnnotationResult.comments
                                    , syntax = typeAnnotationResult.syntax
                                    }
                    )
                    (Layout.maybeLayoutUntilIgnored CustomParser.token "->" |> CustomParser.backtrackable)
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep (CustomParser.lazy (\() -> typeAnnotation))
                , CustomParser.succeed Nothing
                ]
            )


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    CustomParser.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    CustomParser.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


parensTypeAnnotation : Parser (WithComments TypeAnnotation)
parensTypeAnnotation =
    Tokens.parensStart
        |> CustomParser.Extra.continueWith
            (CustomParser.oneOf
                [ Tokens.parensEnd
                    |> CustomParser.map (\() -> unitWithComments)
                , CustomParser.map
                    (\commentsBeforeFirstPart ->
                        \firstPart ->
                            \commentsAfterFirstPart ->
                                \lastToSecondPart ->
                                    { comments =
                                        commentsBeforeFirstPart
                                            |> Rope.prependTo firstPart.comments
                                            |> Rope.prependTo commentsAfterFirstPart
                                            |> Rope.prependTo lastToSecondPart.comments
                                    , syntax =
                                        case lastToSecondPart.syntax of
                                            [] ->
                                                let
                                                    (Node _ firstPartValue) =
                                                        firstPart.syntax
                                                in
                                                firstPartValue

                                            _ ->
                                                TypeAnnotation.Tupled (firstPart.syntax :: List.reverse lastToSecondPart.syntax)
                                    }
                    )
                    Layout.maybeLayout
                    |> CustomParser.keep typeAnnotation
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep
                        (ParserWithComments.untilWithoutReverse
                            Tokens.parensEnd
                            (CustomParser.map
                                (\() ->
                                    \commentsBefore ->
                                        \typeAnnotationResult ->
                                            \commentsAfter ->
                                                { comments =
                                                    commentsBefore
                                                        |> Rope.prependTo typeAnnotationResult.comments
                                                        |> Rope.prependTo commentsAfter
                                                , syntax = typeAnnotationResult.syntax
                                                }
                                )
                                Tokens.comma
                                |> CustomParser.keep Layout.maybeLayout
                                |> CustomParser.keep typeAnnotation
                                |> CustomParser.keep Layout.maybeLayout
                            )
                        )
                ]
            )


unitWithComments : WithComments TypeAnnotation
unitWithComments =
    { comments = Rope.empty, syntax = TypeAnnotation.Unit }


genericTypeAnnotation : Parser (WithComments TypeAnnotation)
genericTypeAnnotation =
    Tokens.functionName
        |> CustomParser.map (\var -> { comments = Rope.empty, syntax = TypeAnnotation.GenericType var })


recordTypeAnnotation : Parser (WithComments TypeAnnotation)
recordTypeAnnotation =
    CustomParser.map
        (\() ->
            \commentsBefore ->
                \afterCurly ->
                    case afterCurly of
                        Nothing ->
                            { comments = commentsBefore
                            , syntax = typeAnnotationRecordEmpty
                            }

                        Just afterCurlyResult ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo afterCurlyResult.comments
                            , syntax = afterCurlyResult.syntax
                            }
        )
        Tokens.curlyStart
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
                    (\( firstNameStartRow, firstNameStartColumn ) ->
                        \firstName ->
                            \commentsAfterFirstName ->
                                \afterFirstName ->
                                    let
                                        firstNameNode : Node String
                                        firstNameNode =
                                            Node.singleLineStringFrom
                                                { row = firstNameStartRow, column = firstNameStartColumn }
                                                firstName
                                    in
                                    Just
                                        { comments =
                                            commentsAfterFirstName
                                                |> Rope.prependTo afterFirstName.comments
                                        , syntax =
                                            case afterFirstName.syntax of
                                                RecordExtensionExpressionAfterName fields ->
                                                    TypeAnnotation.GenericRecord firstNameNode fields

                                                FieldsAfterName fieldsAfterName ->
                                                    TypeAnnotation.Record (Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                                        }
                    )
                    CustomParser.getPosition
                    |> CustomParser.keep Tokens.functionName
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep
                        (CustomParser.oneOf
                            [ Tokens.pipe
                                |> CustomParser.Extra.continueWith
                                    (Node.parserMap
                                        RecordExtensionExpressionAfterName
                                        recordFieldsTypeAnnotation
                                    )
                            , CustomParser.map
                                (\() ->
                                    \commentsBeforeFirstFieldValue ->
                                        \firstFieldValue ->
                                            \commentsAfterFirstFieldValue ->
                                                \tailFields ->
                                                    { comments =
                                                        commentsBeforeFirstFieldValue
                                                            |> Rope.prependTo firstFieldValue.comments
                                                            |> Rope.prependTo commentsAfterFirstFieldValue
                                                            |> Rope.prependTo tailFields.comments
                                                    , syntax =
                                                        FieldsAfterName
                                                            { firstFieldValue = firstFieldValue.syntax
                                                            , tailFields = tailFields.syntax
                                                            }
                                                    }
                                )
                                Tokens.colon
                                |> CustomParser.keep Layout.maybeLayout
                                |> CustomParser.keep typeAnnotation
                                |> CustomParser.keep Layout.maybeLayout
                                |> CustomParser.keep
                                    (CustomParser.oneOf
                                        [ Tokens.comma
                                            |> CustomParser.Extra.continueWith recordFieldsTypeAnnotation
                                        , CustomParser.succeed { comments = Rope.empty, syntax = [] }
                                        ]
                                    )
                            ]
                        )
                    |> CustomParser.ignore Tokens.curlyEnd
                , CustomParser.map (\() -> Nothing)
                    Tokens.curlyEnd
                ]
            )


typeAnnotationRecordEmpty : TypeAnnotation
typeAnnotationRecordEmpty =
    TypeAnnotation.Record []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserWithComments.sepBy1 ","
        (CustomParser.map
            (\commentsBefore ->
                \fields ->
                    { comments = commentsBefore |> Rope.prependTo fields.comments
                    , syntax = fields.syntax
                    }
            )
            Layout.maybeLayout
            |> CustomParser.keep (Node.parser recordFieldDefinition)
        )


recordFieldDefinition : Parser (WithComments TypeAnnotation.RecordField)
recordFieldDefinition =
    CustomParser.map
        (\commentsBeforeFunctionName ->
            \( nameStartRow, nameStartColumn ) ->
                \name ->
                    \commentsAfterFunctionName ->
                        \commentsAfterColon ->
                            \value ->
                                \commentsAfterValue ->
                                    { comments =
                                        commentsBeforeFunctionName
                                            |> Rope.prependTo commentsAfterFunctionName
                                            |> Rope.prependTo commentsAfterColon
                                            |> Rope.prependTo value.comments
                                            |> Rope.prependTo commentsAfterValue
                                    , syntax =
                                        ( Node.singleLineStringFrom
                                            { row = nameStartRow, column = nameStartColumn }
                                            name
                                        , value.syntax
                                        )
                                    }
        )
        Layout.maybeLayout
        |> CustomParser.keep CustomParser.getPosition
        |> CustomParser.keep Tokens.functionName
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token ":")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        |> CustomParser.keep Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithoutArguments =
    CustomParser.map
        (\( nameStartRow, nameStartColumn ) ->
            \startName ->
                \afterStartName ->
                    \nameEndColumn ->
                        { comments = Rope.empty
                        , syntax =
                            TypeAnnotation.Typed
                                (Node
                                    { start = { row = nameStartRow, column = nameStartColumn }
                                    , end = { row = nameStartRow, column = nameEndColumn }
                                    }
                                    (case afterStartName of
                                        Nothing ->
                                            ( [], startName )

                                        Just ( qualificationAfterStartName, unqualified ) ->
                                            ( startName :: qualificationAfterStartName, unqualified )
                                    )
                                )
                                []
                        }
        )
        CustomParser.getPosition
        |> CustomParser.keep Tokens.typeName
        |> CustomParser.keep maybeDotTypeNamesTuple
        |> CustomParser.keep CustomParser.getCol


maybeDotTypeNamesTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    CustomParser.oneOf
        [ CustomParser.map
            (\() ->
                \firstName ->
                    \afterFirstName ->
                        case afterFirstName of
                            Nothing ->
                                Just ( [], firstName )

                            Just ( qualificationAfter, unqualified ) ->
                                Just ( firstName :: qualificationAfter, unqualified )
            )
            Tokens.dot
            |> CustomParser.keep Tokens.typeName
            |> CustomParser.keep (CustomParser.lazy (\() -> maybeDotTypeNamesTuple))
        , CustomParser.succeed Nothing
        ]


typedTypeAnnotationWithArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithArguments =
    CustomParser.map
        (\( nameStartRow, nameStartColumn ) ->
            \startName ->
                \afterStartName ->
                    \nameEndColumn ->
                        \args ->
                            { comments = args.comments
                            , syntax =
                                TypeAnnotation.Typed
                                    (Node
                                        { start = { row = nameStartRow, column = nameStartColumn }
                                        , end = { row = nameStartRow, column = nameEndColumn }
                                        }
                                        (case afterStartName of
                                            Nothing ->
                                                ( [], startName )

                                            Just ( qualificationAfterStartName, unqualified ) ->
                                                ( startName :: qualificationAfterStartName, unqualified )
                                        )
                                    )
                                    args.syntax
                            }
        )
        CustomParser.getPosition
        |> CustomParser.keep Tokens.typeName
        |> CustomParser.keep maybeDotTypeNamesTuple
        |> CustomParser.keep CustomParser.getCol
        |> CustomParser.keep
            (ParserWithComments.many
                (CustomParser.map
                    (\commentsBefore ->
                        \typeAnnotationResult ->
                            { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                            , syntax = typeAnnotationResult.syntax
                            }
                    )
                    (Layout.maybeLayout |> CustomParser.backtrackable)
                    |> CustomParser.keep typeAnnotationNoFnExcludingTypedWithArguments
                )
            )
