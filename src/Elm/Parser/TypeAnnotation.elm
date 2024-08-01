module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    Parser.map
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
        (Parser.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        |= Parser.oneOf
            [ Parser.map
                (\commentsBeforeArrow ->
                    \typeAnnotationResult ->
                        Just
                            { comments =
                                commentsBeforeArrow
                                    |> Rope.prependTo typeAnnotationResult.comments
                            , syntax = typeAnnotationResult.syntax
                            }
                )
                (Layout.maybeLayoutUntilIgnored Tokens.arrowRight |> Parser.backtrackable)
                |= Layout.maybeLayoutUntilWithComments
                    (Parser.lazy (\() -> typeAnnotation))
            , Parser.succeed Nothing
            ]


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    Parser.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    Parser.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


parensTypeAnnotation : Parser (WithComments TypeAnnotation)
parensTypeAnnotation =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ Tokens.parensEnd
                    |> Parser.map (\() -> unitWithComments)
                , Parser.map
                    (\firstPart ->
                        \lastToSecondPart ->
                            { comments =
                                firstPart.comments
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
                    (Layout.maybeLayoutUntilWithComments typeAnnotation)
                    |= Layout.maybeLayoutUntilWithComments
                        (ParserWithComments.untilWithoutReverse Tokens.parensEnd
                            ((Tokens.comma
                                |> Parser.Extra.continueWith
                                    (Parser.map
                                        (\typeAnnotationResult ->
                                            \commentsAfter ->
                                                { comments =
                                                    typeAnnotationResult.comments
                                                        |> Rope.prependTo commentsAfter
                                                , syntax = typeAnnotationResult.syntax
                                                }
                                        )
                                        (Layout.maybeLayoutUntilWithComments typeAnnotation)
                                    )
                             )
                                |= Layout.maybeLayout
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
        |> Parser.map (\var -> { comments = Rope.empty, syntax = TypeAnnotation.GenericType var })


recordTypeAnnotation : Parser (WithComments TypeAnnotation)
recordTypeAnnotation =
    Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Layout.maybeLayoutUntilWithComments
                (Parser.oneOf
                    [ Parser.map
                        (\( firstNameStartRow, firstNameStartColumn ) ->
                            \firstName ->
                                \afterFirstName ->
                                    let
                                        firstNameNode : Node String
                                        firstNameNode =
                                            Node.singleLineStringFrom
                                                { row = firstNameStartRow, column = firstNameStartColumn }
                                                firstName
                                    in
                                    { comments = afterFirstName.comments
                                    , syntax =
                                        case afterFirstName.syntax of
                                            RecordExtensionExpressionAfterName fields ->
                                                TypeAnnotation.GenericRecord firstNameNode fields

                                            FieldsAfterName fieldsAfterName ->
                                                TypeAnnotation.Record (Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                                    }
                        )
                        Parser.getPosition
                        |= Tokens.functionName
                        |= Layout.maybeLayoutUntilWithComments
                            (Parser.oneOf
                                [ Tokens.pipe
                                    |> Parser.Extra.continueWith
                                        (Node.parserMap
                                            RecordExtensionExpressionAfterName
                                            recordFieldsTypeAnnotation
                                        )
                                , (Tokens.colon
                                    |> Parser.Extra.continueWith
                                        (Parser.map
                                            (\firstFieldValue ->
                                                \commentsAfterFirstFieldValue ->
                                                    \tailFields ->
                                                        { comments =
                                                            firstFieldValue.comments
                                                                |> Rope.prependTo commentsAfterFirstFieldValue
                                                                |> Rope.prependTo tailFields.comments
                                                        , syntax =
                                                            FieldsAfterName
                                                                { firstFieldValue = firstFieldValue.syntax
                                                                , tailFields = tailFields.syntax
                                                                }
                                                        }
                                            )
                                            (Layout.maybeLayoutUntilWithComments typeAnnotation)
                                        )
                                  )
                                    |= Layout.maybeLayout
                                    |= Parser.oneOf
                                        [ Tokens.comma
                                            |> Parser.Extra.continueWith recordFieldsTypeAnnotation
                                        , Parser.succeed { comments = Rope.empty, syntax = [] }
                                        ]
                                ]
                            )
                        |. Tokens.curlyEnd
                    , Parser.map (\() -> typeAnnotationRecordEmptyWithComments) Tokens.curlyEnd
                    ]
                )
            )


typeAnnotationRecordEmptyWithComments : WithComments TypeAnnotation
typeAnnotationRecordEmptyWithComments =
    { comments = Rope.empty, syntax = TypeAnnotation.Record [] }


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserWithComments.sepBy1 ","
        (Layout.maybeLayoutUntilWithComments
            (Node.parser recordFieldDefinition)
        )


recordFieldDefinition : Parser (WithComments TypeAnnotation.RecordField)
recordFieldDefinition =
    Parser.map
        (\commentsBeforeFunctionName ->
            \( nameStartRow, nameStartColumn ) ->
                \name ->
                    \commentsAfterFunctionName ->
                        \value ->
                            \commentsAfterValue ->
                                { comments =
                                    commentsBeforeFunctionName
                                        |> Rope.prependTo commentsAfterFunctionName
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
        |= Parser.getPosition
        |= Tokens.functionName
        |= Layout.maybeLayoutUntilIgnored Tokens.colon
        |= Layout.maybeLayoutUntilWithComments typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        |= Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithoutArguments =
    Parser.map
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
        Parser.getPosition
        |= Tokens.typeName
        |= maybeDotTypeNamesTuple
        |= Parser.getCol


maybeDotTypeNamesTuple : Parser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    Parser.oneOf
        [ (Tokens.dot
            |> Parser.Extra.continueWith
                (Parser.map
                    (\firstName ->
                        \afterFirstName ->
                            case afterFirstName of
                                Nothing ->
                                    Just ( [], firstName )

                                Just ( qualificationAfter, unqualified ) ->
                                    Just ( firstName :: qualificationAfter, unqualified )
                    )
                    Tokens.typeName
                )
          )
            |= Parser.lazy (\() -> maybeDotTypeNamesTuple)
        , Parser.succeed Nothing
        ]


typedTypeAnnotationWithArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithArguments =
    Parser.map
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
        Parser.getPosition
        |= Tokens.typeName
        |= maybeDotTypeNamesTuple
        |= Parser.getCol
        |= ParserWithComments.many
            (Parser.map
                (\commentsBefore ->
                    \typeAnnotationResult ->
                        { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                        , syntax = typeAnnotationResult.syntax
                        }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= typeAnnotationNoFnExcludingTypedWithArguments
            )
