module Elm.Parser.Declarations exposing (declaration)

import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression)
import Elm.Parser.Layout as Layout
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope


declaration : Parser (WithComments (Node Declaration))
declaration =
    ParserFast.oneOf5
        functionDeclarationWithoutDocumentation
        declarationWithDocumentation
        typeOrTypeAliasDefinitionWithoutDocumentation
        portDeclarationWithoutDocumentation
        infixDeclaration


declarationWithDocumentation : Parser (WithComments (Node Declaration))
declarationWithDocumentation =
    ParserFast.map2
        (\documentation ( afterDocumentationComments, afterDocumentation ) ->
            let
                start : Location
                start =
                    (Node.range documentation).start
            in
            case afterDocumentation of
                FunctionDeclarationAfterDocumentation functionDeclarationAfterDocumentation ->
                    case functionDeclarationAfterDocumentation.signature of
                        Just signature ->
                            let
                                (Node implementationNameRange _) =
                                    signature.implementationName

                                (Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            ( afterDocumentationComments
                            , Node { start = start, end = expressionRange.end }
                                (Declaration.FunctionDeclaration
                                    { documentation = Just documentation
                                    , signature =
                                        Just
                                            (Node.combine Signature
                                                functionDeclarationAfterDocumentation.startName
                                                signature.typeAnnotation
                                            )
                                    , declaration =
                                        Node { start = implementationNameRange.start, end = expressionRange.end }
                                            { name = signature.implementationName
                                            , arguments = functionDeclarationAfterDocumentation.arguments
                                            , expression = functionDeclarationAfterDocumentation.expression
                                            }
                                    }
                                )
                            )

                        Nothing ->
                            let
                                (Node startNameRange _) =
                                    functionDeclarationAfterDocumentation.startName

                                (Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            ( afterDocumentationComments
                            , Node { start = start, end = expressionRange.end }
                                (Declaration.FunctionDeclaration
                                    { documentation = Just documentation
                                    , signature = Nothing
                                    , declaration =
                                        Node { start = startNameRange.start, end = expressionRange.end }
                                            { name = functionDeclarationAfterDocumentation.startName
                                            , arguments = functionDeclarationAfterDocumentation.arguments
                                            , expression = functionDeclarationAfterDocumentation.expression
                                            }
                                    }
                                )
                            )

                TypeDeclarationAfterDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    ( afterDocumentationComments
                    , Node { start = start, end = end }
                        (Declaration.CustomTypeDeclaration
                            { documentation = Just documentation
                            , name = typeDeclarationAfterDocumentation.name
                            , generics = typeDeclarationAfterDocumentation.parameters
                            , constructors =
                                typeDeclarationAfterDocumentation.headVariant
                                    :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                            }
                        )
                    )

                TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    ( afterDocumentationComments
                    , Node { start = start, end = typeAnnotationRange.end }
                        (Declaration.AliasDeclaration
                            { documentation = Just documentation
                            , name = typeAliasDeclarationAfterDocumentation.name
                            , generics = typeAliasDeclarationAfterDocumentation.parameters
                            , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                            }
                        )
                    )

                PortDeclarationAfterDocumentation portDeclarationAfterName ->
                    let
                        (Node typeAnnotationRange _) =
                            portDeclarationAfterName.typeAnnotation
                    in
                    ( Rope.one documentation
                        |> Rope.filledPrependTo afterDocumentationComments
                    , Node
                        { start = portDeclarationAfterName.startLocation
                        , end = typeAnnotationRange.end
                        }
                        (Declaration.PortDeclaration
                            { name = portDeclarationAfterName.name
                            , typeAnnotation = portDeclarationAfterName.typeAnnotation
                            }
                        )
                    )
        )
        Comments.declarationDocumentation
        (Layout.layoutStrictFollowedByWithComments
            (ParserFast.oneOf3
                functionAfterDocumentation
                typeOrTypeAliasDefinitionAfterDocumentation
                portDeclarationAfterDocumentation
            )
        )
        |> ParserFast.validate
            (\result ->
                let
                    (Node _ decl) =
                        result |> Tuple.second
                in
                case decl of
                    Declaration.FunctionDeclaration letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Node _ signature) ->
                                let
                                    (Node _ implementationName) =
                                        implementation.name

                                    (Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName ++ ""

                    _ ->
                        True
            )
            "Expected to find the same name for declaration and signature"


type DeclarationAfterDocumentation
    = FunctionDeclarationAfterDocumentation
        { startName : Node String
        , signature :
            Maybe
                { typeAnnotation : Node TypeAnnotation
                , implementationName : Node String
                }
        , arguments : List (Node Pattern)
        , expression : Node Expression
        }
    | TypeDeclarationAfterDocumentation
        { name : Node String
        , parameters : List (Node String)
        , headVariant : Node ValueConstructor
        , tailVariantsReverse : List (Node ValueConstructor)
        }
    | TypeAliasDeclarationAfterDocumentation
        { name : Node String
        , parameters : List (Node String)
        , typeAnnotation : Node TypeAnnotation
        }
    | PortDeclarationAfterDocumentation
        { startLocation : Location
        , name : Node String
        , typeAnnotation : Node TypeAnnotation
        }


type TypeOrTypeAliasDeclarationWithoutDocumentation
    = TypeDeclarationWithoutDocumentation
        { name : Node String
        , parameters : List (Node String)
        , headVariant : Node ValueConstructor
        , tailVariantsReverse : List (Node ValueConstructor)
        }
    | TypeAliasDeclarationWithoutDocumentation
        { name : Node String
        , parameters : List (Node String)
        , typeAnnotation : Node TypeAnnotation
        }


functionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
functionAfterDocumentation =
    ParserFast.map6
        (\startName commentsAfterStartName ( maybeSignatureComments, maybeSignature ) ( argumentsComments, arguments ) commentsAfterEqual ( resultComments, result ) ->
            ( commentsAfterStartName
                |> Rope.prependTo maybeSignatureComments
                |> Rope.prependTo argumentsComments
                |> Rope.prependTo commentsAfterEqual
                |> Rope.prependTo resultComments
            , FunctionDeclarationAfterDocumentation
                { startName = startName
                , signature = maybeSignature
                , arguments = arguments
                , expression = result
                }
            )
        )
        -- infix declarations itself don't have documentation
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation ( typeAnnotationComments, typeAnnotationResult ) ( implementationNameComments, implementationName ) afterImplementationName ->
                ( commentsBeforeTypeAnnotation
                    |> Rope.prependTo typeAnnotationComments
                    |> Rope.prependTo implementationNameComments
                    |> Rope.prependTo afterImplementationName
                , Just
                    { implementationName = implementationName
                    , typeAnnotation = typeAnnotationResult
                    }
                )
            )
            (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
            TypeAnnotation.typeAnnotation
            (Layout.layoutStrictFollowedBy
                Tokens.functionNameNode
            )
            Layout.maybeLayout
            ( Rope.empty, Nothing )
        )
        parameterPatternsEqual
        Layout.maybeLayout
        expression


functionDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
functionDeclarationWithoutDocumentation =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature ( argumentsComments, arguments ) commentsAfterEqual ( resultComments, result ) ->
            let
                allComments : Comments
                allComments =
                    (case maybeSignature of
                        Nothing ->
                            commentsAfterStartName

                        Just signature ->
                            commentsAfterStartName |> Rope.prependTo signature.comments
                    )
                        |> Rope.prependTo argumentsComments
                        |> Rope.prependTo commentsAfterEqual
                        |> Rope.prependTo resultComments
            in
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            result
                    in
                    ( allComments
                    , Node { start = startNameStart, end = expressionRange.end }
                        (Declaration.FunctionDeclaration
                            { documentation = Nothing
                            , signature = Nothing
                            , declaration =
                                Node { start = startNameStart, end = expressionRange.end }
                                    { name = startNameNode
                                    , arguments = arguments
                                    , expression = result
                                    }
                            }
                        )
                    )

                Just signature ->
                    let
                        (Node implementationNameRange _) =
                            signature.implementationName

                        (Node expressionRange _) =
                            result
                    in
                    ( allComments
                    , Node { start = startNameStart, end = expressionRange.end }
                        (Declaration.FunctionDeclaration
                            { documentation = Nothing
                            , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                            , declaration =
                                Node { start = implementationNameRange.start, end = expressionRange.end }
                                    { name = signature.implementationName
                                    , arguments = arguments
                                    , expression = result
                                    }
                            }
                        )
                    )
        )
        Tokens.functionNameNotInfixNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation ( typeAnnotationComments, typeAnnotationResult ) ( implementationNameComments, implementationName ) afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationComments
                            |> Rope.prependTo implementationNameComments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName
                    , typeAnnotation = typeAnnotationResult
                    }
            )
            (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
            TypeAnnotation.typeAnnotation
            (Layout.layoutStrictFollowedBy
                Tokens.functionNameNode
            )
            Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Layout.maybeLayout
        expression
        |> ParserFast.validate
            (\result ->
                let
                    (Node _ decl) =
                        result |> Tuple.second
                in
                case decl of
                    Declaration.FunctionDeclaration letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Node _ signature) ->
                                let
                                    (Node _ implementationName) =
                                        implementation.name

                                    (Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName ++ ""

                    _ ->
                        True
            )
            "Expected to find the same name for declaration and signature"


parameterPatternsEqual : Parser (WithComments (List (Node Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                ( patternResult |> Tuple.first |> Rope.prependTo commentsAfterPattern
                , patternResult |> Tuple.second
                )
            )
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
        )


infixDeclaration : Parser (WithComments (Node Declaration))
infixDeclaration =
    ParserFast.map9WithRange
        (\range commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual fn ->
            ( commentsAfterInfix
                |> Rope.prependTo commentsAfterDirection
                |> Rope.prependTo commentsAfterPrecedence
                |> Rope.prependTo commentsAfterOperator
                |> Rope.prependTo commentsAfterEqual
            , Node range
                (Declaration.InfixDeclaration
                    { direction = direction, precedence = precedence, operator = operator, function = fn }
                )
            )
        )
        (ParserFast.keywordFollowedBy "infix" Layout.maybeLayout)
        infixDirection
        Layout.maybeLayout
        (ParserFast.integerDecimalMapWithRange Node)
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "("
            (ParserFast.whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                (\operatorRange operator ->
                    Node
                        { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                        , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                        }
                        operator
                )
                Tokens.isOperatorSymbolChar
                Tokens.isAllowedOperatorToken
                ")"
            )
        )
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        Tokens.functionNameNode


infixDirection : ParserFast.Parser (Node Infix.InfixDirection)
infixDirection =
    ParserFast.oneOf3
        (ParserFast.mapWithRange Node (ParserFast.keyword "right" Infix.Right))
        (ParserFast.mapWithRange Node (ParserFast.keyword "left" Infix.Left))
        (ParserFast.mapWithRange Node (ParserFast.keyword "non" Infix.Non))


portDeclarationAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    ParserFast.map5
        (\commentsAfterPort ((Node nameRange _) as name) commentsAfterName commentsAfterColon typeAnnotationResult ->
            ( commentsAfterPort
                |> Rope.prependTo commentsAfterName
                |> Rope.prependTo (typeAnnotationResult |> Tuple.first)
                |> Rope.prependTo commentsAfterColon
            , PortDeclarationAfterDocumentation
                { startLocation = { row = nameRange.start.row, column = 1 }
                , name = name
                , typeAnnotation = typeAnnotationResult |> Tuple.second
                }
            )
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
        typeAnnotation


portDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
portDeclarationWithoutDocumentation =
    ParserFast.map5
        (\commentsAfterPort ((Node nameRange _) as name) commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (Node { end } _) =
                    typeAnnotationResult |> Tuple.second
            in
            ( commentsAfterPort
                |> Rope.prependTo commentsAfterName
                |> Rope.prependTo commentsAfterColon
                |> Rope.prependTo (typeAnnotationResult |> Tuple.first)
            , Node
                { start = { row = nameRange.start.row, column = 1 }
                , end = end
                }
                (Declaration.PortDeclaration
                    { name = name
                    , typeAnnotation = typeAnnotationResult |> Tuple.second
                    }
                )
            )
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
        typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    ParserFast.map2
        (\commentsAfterType declarationAfterDocumentation ->
            ( commentsAfterType |> Rope.prependTo (declarationAfterDocumentation |> Tuple.first)
            , declarationAfterDocumentation |> Tuple.second
            )
        )
        (ParserFast.keywordFollowedBy "type" Layout.maybeLayout)
        (ParserFast.oneOf2
            typeAliasDefinitionAfterDocumentationAfterTypePrefix
            customTypeDefinitionAfterDocumentationAfterTypePrefix
        )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEquals typeAnnotationResult ->
            ( commentsAfterAlias
                |> Rope.prependTo commentsAfterName
                |> Rope.prependTo (parameters |> Tuple.first)
                |> Rope.prependTo commentsAfterEquals
                |> Rope.prependTo (typeAnnotationResult |> Tuple.first)
            , TypeAliasDeclarationAfterDocumentation
                { name = name
                , parameters = parameters |> Tuple.second
                , typeAnnotation = typeAnnotationResult |> Tuple.second
                }
            )
        )
        (ParserFast.keywordFollowedBy "alias" Layout.maybeLayout)
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        typeAnnotation


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\name commentsAfterName ( parametersComments, parameters ) commentsAfterEqual ( headVariantComments, headVariant ) ( tailVariantsComments, tailVariantsReverse ) ->
            ( commentsAfterName
                |> Rope.prependTo parametersComments
                |> Rope.prependTo commentsAfterEqual
                |> Rope.prependTo headVariantComments
                |> Rope.prependTo tailVariantsComments
            , TypeDeclarationAfterDocumentation
                { name = name
                , parameters = parameters
                , headVariant = headVariant
                , tailVariantsReverse = tailVariantsReverse
                }
            )
        )
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        valueConstructorOptimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.symbolFollowedBy "|"
                (Layout.positivelyIndentedPlusFollowedBy 1
                    (ParserFast.map2
                        (\commentsBeforePipe variantResult ->
                            ( commentsBeforePipe
                                |> Rope.prependTo (variantResult |> Tuple.first)
                            , variantResult |> Tuple.second
                            )
                        )
                        Layout.maybeLayout
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (Node Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    ParserFast.map2WithStartLocation
        (\start commentsAfterType afterStart ->
            let
                allComments : Comments
                allComments =
                    commentsAfterType |> Rope.prependTo (afterStart |> Tuple.first)
            in
            case afterStart |> Tuple.second of
                TypeDeclarationWithoutDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    ( allComments
                    , Node { start = start, end = end }
                        (Declaration.CustomTypeDeclaration
                            { documentation = Nothing
                            , name = typeDeclarationAfterDocumentation.name
                            , generics = typeDeclarationAfterDocumentation.parameters
                            , constructors =
                                typeDeclarationAfterDocumentation.headVariant
                                    :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                            }
                        )
                    )

                TypeAliasDeclarationWithoutDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    ( allComments
                    , Node { start = start, end = typeAnnotationRange.end }
                        (Declaration.AliasDeclaration
                            { documentation = Nothing
                            , name = typeAliasDeclarationAfterDocumentation.name
                            , generics = typeAliasDeclarationAfterDocumentation.parameters
                            , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                            }
                        )
                    )
        )
        (ParserFast.keywordFollowedBy "type"
            Layout.maybeLayout
        )
        (ParserFast.oneOf2
            typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            customTypeDefinitionWithoutDocumentationAfterTypePrefix
        )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEqual typeAnnotationResult ->
            ( commentsAfterAlias
                |> Rope.prependTo commentsAfterName
                |> Rope.prependTo (parameters |> Tuple.first)
                |> Rope.prependTo commentsAfterEqual
                |> Rope.prependTo (typeAnnotationResult |> Tuple.first)
            , TypeAliasDeclarationWithoutDocumentation
                { name = name
                , parameters = parameters |> Tuple.second
                , typeAnnotation = typeAnnotationResult |> Tuple.second
                }
            )
        )
        (ParserFast.keywordFollowedBy "alias" Layout.maybeLayout)
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        typeAnnotation


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\name commentsAfterName ( parametersComments, parameters ) commentsAfterEqual ( headVariantComments, headVariant ) ( tailVariantsComments, tailVariantsReverse ) ->
            ( commentsAfterName
                |> Rope.prependTo parametersComments
                |> Rope.prependTo commentsAfterEqual
                |> Rope.prependTo headVariantComments
                |> Rope.prependTo tailVariantsComments
            , TypeDeclarationWithoutDocumentation
                { name = name
                , parameters = parameters
                , headVariant = headVariant
                , tailVariantsReverse = tailVariantsReverse
                }
            )
        )
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        valueConstructorOptimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.symbolFollowedBy "|"
                (Layout.positivelyIndentedPlusFollowedBy 1
                    (ParserFast.map2
                        (\commentsBeforePipe variantResult ->
                            ( commentsBeforePipe
                                |> Rope.prependTo (variantResult |> Tuple.first)
                            , variantResult |> Tuple.second
                            )
                        )
                        Layout.maybeLayout
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


valueConstructorOptimisticLayout : Parser (WithComments (Node ValueConstructor))
valueConstructorOptimisticLayout =
    ParserFast.map3
        (\((Node nameRange _) as name) commentsAfterName argumentsReverse ->
            let
                fullRange : Range
                fullRange =
                    case argumentsReverse |> Tuple.second of
                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }

                        [] ->
                            nameRange
            in
            ( commentsAfterName
                |> Rope.prependTo (argumentsReverse |> Tuple.first)
            , Node fullRange
                { name = name
                , arguments = List.reverse (argumentsReverse |> Tuple.second)
                }
            )
        )
        Tokens.typeNameNode
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        ( typeAnnotationResult |> Tuple.first |> Rope.prependTo commentsAfter
                        , typeAnnotationResult |> Tuple.second
                        )
                    )
                    typeAnnotationNoFnExcludingTypedWithArguments
                    Layout.optimisticLayout
                )
            )
        )


typeGenericListEquals : Parser (WithComments (List (Node String)))
typeGenericListEquals =
    ParserWithComments.until Tokens.equal
        (ParserFast.map2
            (\name commentsAfterName ->
                ( commentsAfterName
                , name
                )
            )
            Tokens.functionNameNode
            Layout.maybeLayout
        )
