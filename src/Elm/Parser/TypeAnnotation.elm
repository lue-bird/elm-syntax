module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
        (ParserFast.map2
            (\( startTypeComments, startTypeSyntax ) commentsAfter ->
                ( startTypeComments
                    |> Rope.prependTo commentsAfter
                , startTypeSyntax
                )
            )
            (ParserFast.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
            Layout.optimisticLayout
        )
        (ParserFast.symbolFollowedBy "->"
            (Layout.positivelyIndentedPlusFollowedBy 2
                (ParserFast.map3
                    (\commentsAfterArrow typeAnnotationResult commentsAfterType ->
                        ( commentsAfterArrow
                            |> Rope.prependTo (typeAnnotationResult |> Tuple.first)
                            |> Rope.prependTo commentsAfterType
                        , typeAnnotationResult |> Tuple.second
                        )
                    )
                    Layout.maybeLayout
                    (ParserFast.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
                    Layout.optimisticLayout
                )
            )
        )
        (\inType outType ->
            ( inType
                |> Tuple.first
                |> Rope.prependTo (outType |> Tuple.first)
            , Node.combine TypeAnnotation.FunctionTypeAnnotation (inType |> Tuple.second) (outType |> Tuple.second)
            )
        )


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithoutArguments
        genericTypeAnnotation
        recordTypeAnnotation


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithArgumentsOptimisticLayout
        genericTypeAnnotation
        recordTypeAnnotation


parensTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
parensTypeAnnotation =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    ( Rope.empty
                    , Node
                        { start = { row = end.row, column = end.column - 2 }
                        , end = end
                        }
                        TypeAnnotation.Unit
                    )
                )
            )
            (ParserFast.map4WithRange
                (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart commentsAfterFirstPart lastToSecondPart ->
                    ( commentsBeforeFirstPart
                        |> Rope.prependTo (firstPart |> Tuple.first)
                        |> Rope.prependTo commentsAfterFirstPart
                        |> Rope.prependTo (lastToSecondPart |> Tuple.first)
                    , Node
                        { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                        , end = rangeAfterOpeningParens.end
                        }
                        (case lastToSecondPart |> Tuple.second of
                            Nothing ->
                                -- parenthesized types are not a `Tupled [ firstPart |> Tuple.second ]`
                                -- but their Range still extends to both parens.
                                -- This is done to not break behavior of v7.
                                -- This will likely change in v8 after discussion in issues like https://github.com/stil4m/elm-syntax/issues/204
                                let
                                    (Node _ firstPartType) =
                                        firstPart |> Tuple.second
                                in
                                firstPartType

                            Just firstAndMaybeThirdPart ->
                                case firstAndMaybeThirdPart.maybeThirdPart of
                                    Nothing ->
                                        TypeAnnotation.Tupled [ firstPart |> Tuple.second, firstAndMaybeThirdPart.secondPart ]

                                    Just thirdPart ->
                                        TypeAnnotation.Tupled [ firstPart |> Tuple.second, firstAndMaybeThirdPart.secondPart, thirdPart ]
                        )
                    )
                )
                Layout.maybeLayout
                typeAnnotation
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbol ")"
                        ( Rope.empty, Nothing )
                    )
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore secondPartResult commentsAfter maybeThirdPartResult ->
                                ( commentsBefore
                                    |> Rope.prependTo (secondPartResult |> Tuple.first)
                                    |> Rope.prependTo commentsAfter
                                , Just { maybeThirdPart = maybeThirdPartResult |> Tuple.second, secondPart = secondPartResult |> Tuple.second }
                                )
                            )
                            Layout.maybeLayout
                            typeAnnotation
                            Layout.maybeLayout
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")"
                                    ( Rope.empty, Nothing )
                                )
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore thirdPartResult commentsAfter ->
                                            ( commentsBefore
                                                |> Rope.prependTo (thirdPartResult |> Tuple.first)
                                                |> Rope.prependTo commentsAfter
                                            , Just (thirdPartResult |> Tuple.second)
                                            )
                                        )
                                        Layout.maybeLayout
                                        typeAnnotation
                                        Layout.maybeLayout
                                        |> ParserFast.followedBySymbol ")"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


genericTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
genericTypeAnnotation =
    Tokens.functionNameMapWithRange
        (\range var ->
            ( Rope.empty
            , Node range (TypeAnnotation.GenericType var)
            )
        )


recordTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
recordTypeAnnotation =
    ParserFast.map2WithRange
        (\range commentsBefore afterCurly ->
            ( commentsBefore
                |> Rope.prependTo (afterCurly |> Tuple.first)
            , case afterCurly |> Tuple.second of
                Nothing ->
                    Node range typeAnnotationRecordEmpty

                Just afterCurlyResult ->
                    Node range afterCurlyResult
            )
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\firstNameNode commentsAfterFirstName afterFirstName ->
                    ( commentsAfterFirstName
                        |> Rope.prependTo (afterFirstName |> Tuple.first)
                    , Just
                        (case afterFirstName |> Tuple.second of
                            RecordExtensionExpressionAfterName fields ->
                                TypeAnnotation.GenericRecord firstNameNode fields

                            FieldsAfterName fieldsAfterName ->
                                TypeAnnotation.Record (Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                        )
                    )
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map3WithRange
                            (\range commentsBefore head tail ->
                                ( commentsBefore
                                    |> Rope.prependTo (head |> Tuple.first)
                                    |> Rope.prependTo (tail |> Tuple.first)
                                , RecordExtensionExpressionAfterName
                                    (Node range ((head |> Tuple.second) :: (tail |> Tuple.second)))
                                )
                            )
                            Layout.maybeLayout
                            recordFieldDefinition
                            (ParserWithComments.many
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore field ->
                                            ( commentsBefore |> Rope.prependTo (field |> Tuple.first)
                                            , field |> Tuple.second
                                            )
                                        )
                                        Layout.maybeLayout
                                        recordFieldDefinition
                                    )
                                )
                            )
                        )
                    )
                    (ParserFast.symbolFollowedBy ":"
                        (ParserFast.map4
                            (\commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
                                ( commentsBeforeFirstFieldValue
                                    |> Rope.prependTo (firstFieldValue |> Tuple.first)
                                    |> Rope.prependTo commentsAfterFirstFieldValue
                                    |> Rope.prependTo (tailFields |> Tuple.first)
                                , FieldsAfterName
                                    { firstFieldValue = firstFieldValue |> Tuple.second
                                    , tailFields = tailFields |> Tuple.second
                                    }
                                )
                            )
                            Layout.maybeLayout
                            typeAnnotation
                            Layout.maybeLayout
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," recordFieldsTypeAnnotation)
                                ( Rope.empty, [] )
                            )
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" ( Rope.empty, Nothing ))
        )


typeAnnotationRecordEmpty : TypeAnnotation
typeAnnotationRecordEmpty =
    TypeAnnotation.Record []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserFast.map3
        (\commentsBefore head tail ->
            ( commentsBefore
                |> Rope.prependTo (head |> Tuple.first)
                |> Rope.prependTo (tail |> Tuple.first)
            , (head |> Tuple.second) :: (tail |> Tuple.second)
            )
        )
        Layout.maybeLayout
        recordFieldDefinition
        (ParserWithComments.many
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map2
                    (\commentsBefore field ->
                        ( commentsBefore |> Rope.prependTo (field |> Tuple.first)
                        , field |> Tuple.second
                        )
                    )
                    Layout.maybeLayout
                    recordFieldDefinition
                )
            )
        )


recordFieldDefinition : Parser (WithComments (Node TypeAnnotation.RecordField))
recordFieldDefinition =
    ParserFast.map6WithRange
        (\range commentsBeforeFunctionName name commentsAfterFunctionName commentsAfterColon value commentsAfterValue ->
            ( commentsBeforeFunctionName
                |> Rope.prependTo commentsAfterFunctionName
                |> Rope.prependTo commentsAfterColon
                |> Rope.prependTo (value |> Tuple.first)
                |> Rope.prependTo commentsAfterValue
            , Node range ( name, value |> Tuple.second )
            )
        )
        Layout.maybeLayout
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
        typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithoutArguments =
    ParserFast.map2WithRange
        (\range startName afterStartName ->
            let
                name : ( ModuleName, String )
                name =
                    case afterStartName of
                        Nothing ->
                            ( [], startName )

                        Just ( qualificationAfterStartName, unqualified ) ->
                            ( startName :: qualificationAfterStartName, unqualified )
            in
            ( Rope.empty
            , Node range
                (TypeAnnotation.Typed (Node range name) [])
            )
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.map2OrSucceed
        (\firstName afterFirstName ->
            case afterFirstName of
                Nothing ->
                    Just ( [], firstName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( firstName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." Tokens.typeName)
        (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        Nothing


typedTypeAnnotationWithArgumentsOptimisticLayout : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithArgumentsOptimisticLayout =
    ParserFast.map3
        (\((Node nameRange _) as nameNode) commentsAfterName argsReverse ->
            let
                range : Range
                range =
                    case argsReverse |> Tuple.second of
                        [] ->
                            nameRange

                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            ( commentsAfterName
                |> Rope.prependTo (argsReverse |> Tuple.first)
            , Node range (TypeAnnotation.Typed nameNode (List.reverse (argsReverse |> Tuple.second)))
            )
        )
        (ParserFast.map2WithRange
            (\range startName afterStartName ->
                let
                    name : ( ModuleName, String )
                    name =
                        case afterStartName of
                            Nothing ->
                                ( [], startName )

                            Just ( qualificationAfterStartName, unqualified ) ->
                                ( startName :: qualificationAfterStartName, unqualified )
                in
                Node range name
            )
            Tokens.typeName
            maybeDotTypeNamesTuple
        )
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        ( typeAnnotationResult
                            |> Tuple.first
                            |> Rope.prependTo commentsAfter
                        , typeAnnotationResult |> Tuple.second
                        )
                    )
                    typeAnnotationNoFnExcludingTypedWithArguments
                    Layout.optimisticLayout
                )
            )
        )
