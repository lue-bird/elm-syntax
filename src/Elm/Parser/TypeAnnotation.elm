module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Parser as Core exposing ((|.), (|=))


typeAnnotation : Parser State (Node TypeAnnotation)
typeAnnotation =
    typeAnnotationNoFnIncludingTypedWithArgumentsLazy
        |> Combine.andThen
            (\typeRef ->
                Layout.optimisticLayoutWith
                    (\() -> typeRef)
                    (\() ->
                        Combine.oneOf
                            [ arrowRightToTypeAnnotation
                                |> Combine.map (\ta -> Node.combine TypeAnnotation.FunctionTypeAnnotation typeRef ta)
                            , Combine.succeed typeRef
                            ]
                    )
            )


arrowRightToTypeAnnotation : Parser State (Node TypeAnnotation)
arrowRightToTypeAnnotation =
    Tokens.arrowRight
        |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith typeAnnotation


typeAnnotationNoFnExcludingTypedWithArguments : Parser State (Node TypeAnnotation)
typeAnnotationNoFnExcludingTypedWithArguments =
    Combine.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFnIncludingTypedWithArguments : Parser State (Node TypeAnnotation)
typeAnnotationNoFnIncludingTypedWithArguments =
    Combine.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFnIncludingTypedWithArgumentsLazy : Parser State (Node TypeAnnotation)
typeAnnotationNoFnIncludingTypedWithArgumentsLazy =
    Combine.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments)


typeAnnotationNoFnExcludingTypedWithArgumentsLazy : Parser State (Node TypeAnnotation)
typeAnnotationNoFnExcludingTypedWithArgumentsLazy =
    Combine.lazy (\() -> typeAnnotationNoFnExcludingTypedWithArguments)


parensTypeAnnotation : Parser State (Node TypeAnnotation)
parensTypeAnnotation =
    Tokens.parensStart
        |> Combine.continueFromCore
            (Combine.oneOf
                [ Tokens.parensEnd
                    |> Combine.fromCoreMap (\() -> TypeAnnotation.Unit)
                , parensTypeAnnotationInnerNested |> Combine.ignoreEntirely Tokens.parensEnd
                ]
            )
        |> Node.parser


parensTypeAnnotationInnerCommaSep : Parser State (List (Node TypeAnnotation))
parensTypeAnnotationInnerCommaSep =
    Combine.many
        (Tokens.comma
            |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
            |> Combine.continueWith typeAnnotation
            |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        )


parensTypeAnnotationInnerNested : Parser State TypeAnnotation
parensTypeAnnotationInnerNested =
    Combine.succeed (\x -> \xs -> asTypeAnnotation x xs)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep parensTypeAnnotationInnerCommaSep


asTypeAnnotation : Node TypeAnnotation -> List (Node TypeAnnotation) -> TypeAnnotation
asTypeAnnotation ((Node _ value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            TypeAnnotation.Tupled (x :: xs)


genericTypeAnnotation : Parser state (Node TypeAnnotation)
genericTypeAnnotation =
    Tokens.functionName
        |> Core.map TypeAnnotation.GenericType
        |> Node.parserCore
        |> Combine.fromCore


recordFieldsTypeAnnotation : Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    Combine.sepBy1 "," (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition)


recordTypeAnnotation : Parser State (Node TypeAnnotation)
recordTypeAnnotation =
    Tokens.curlyStart
        |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Core.map (\() -> TypeAnnotation.Record []) Tokens.curlyEnd |> Combine.fromCore
                , Node.parserCore Tokens.functionName
                    |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
                    |> Combine.andThen
                        (\fname ->
                            Combine.oneOf
                                [ Combine.map (\fields -> TypeAnnotation.GenericRecord fname fields)
                                    pipeRecordFieldsTypeAnnotationNodeCurlyEnd
                                , Combine.succeed (\ta -> \rest -> TypeAnnotation.Record <| Node.combine Tuple.pair fname ta :: rest)
                                    |> Combine.keep colonTypeAnnotationMaybeLayout
                                    |> Combine.keep maybeCommaRecordFieldsTypeAnnotationCurlyEnd
                                ]
                        )
                ]
            )
        |> Node.parser


pipeRecordFieldsTypeAnnotationNodeCurlyEnd : Parser State (Node TypeAnnotation.RecordDefinition)
pipeRecordFieldsTypeAnnotationNodeCurlyEnd =
    Tokens.pipe
        |> Combine.continueWithFromCore
            (Node.parser recordFieldsTypeAnnotation
                |> Combine.ignoreEntirely Tokens.curlyEnd
            )


colonTypeAnnotationMaybeLayout : Parser State (Node TypeAnnotation)
colonTypeAnnotationMaybeLayout =
    Tokens.colon
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (typeAnnotation
                |> Combine.ignore (Combine.maybeIgnore Layout.layout)
            )


maybeCommaRecordFieldsTypeAnnotationCurlyEnd : Parser State TypeAnnotation.RecordDefinition
maybeCommaRecordFieldsTypeAnnotationCurlyEnd =
    Combine.oneOf
        [ -- Skip a comma and then look for at least 1 more field
          Tokens.comma
            |> Combine.continueFromCore recordFieldsTypeAnnotation
        , -- Single field record, so just end with no additional fields
          Combine.succeed []
        ]
        |> Combine.ignoreEntirely Tokens.curlyEnd


recordFieldDefinition : Parser State TypeAnnotation.RecordField
recordFieldDefinition =
    Combine.succeed (\functionName -> \value -> ( functionName, value ))
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.colon
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation


typedTypeAnnotationWithArguments : Parser State (Node TypeAnnotation)
typedTypeAnnotationWithArguments =
    typeIndicator
        |> Combine.andThenFromCore
            (\((Node tir _) as original) ->
                Layout.optimisticLayoutWith
                    (\() -> Node tir (TypeAnnotation.Typed original []))
                    (\() -> eagerTypedTypeAnnotation original)
            )


typedTypeAnnotationWithoutArguments : Parser State (Node TypeAnnotation)
typedTypeAnnotationWithoutArguments =
    Combine.fromCoreMap
        (\((Node tir _) as original) ->
            Node tir (TypeAnnotation.Typed original [])
        )
        typeIndicator


typeIndicator : Core.Parser (Node ( ModuleName, String ))
typeIndicator =
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> typeIndicatorHelper [] typeOrSegment)
        |> Node.parserCore


typeIndicatorHelper : ModuleName -> String -> Core.Parser ( ModuleName, String )
typeIndicatorHelper moduleNameSoFar typeOrSegment =
    Core.oneOf
        [ dotTypeName
            |> Core.andThen (\t -> typeIndicatorHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed ( List.reverse moduleNameSoFar, typeOrSegment ))
        ]


dotTypeName : Core.Parser String
dotTypeName =
    Core.succeed identity
        |. Tokens.dot
        |= Tokens.typeName


eagerTypedTypeAnnotation : Node ( ModuleName, String ) -> Parser State (Node TypeAnnotation)
eagerTypedTypeAnnotation ((Node range _) as original) =
    eagerTypedTypeAnnotationInnerGenericHelper []
        |> Combine.map
            (\args ->
                let
                    endRange : Range
                    endRange =
                        case args of
                            (Node argRange _) :: _ ->
                                argRange

                            [] ->
                                range
                in
                Node
                    { start = range.start, end = endRange.end }
                    (TypeAnnotation.Typed original (List.reverse args))
            )


eagerTypedTypeAnnotationInnerGenericHelper : List (Node TypeAnnotation) -> Parser State (List (Node TypeAnnotation))
eagerTypedTypeAnnotationInnerGenericHelper items =
    Combine.oneOf
        [ typeAnnotationNoFnExcludingTypedWithArgumentsLazy
            |> Combine.andThen
                (\next ->
                    Layout.optimisticLayoutWith
                        (\() -> next :: items)
                        (\() -> eagerTypedTypeAnnotationInnerGenericHelper (next :: items))
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                )
        , Combine.succeed items
        ]
