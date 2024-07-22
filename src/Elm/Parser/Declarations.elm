module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression, failIfDifferentFrom, functionSignatureFromVarPointer)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings exposing (typeDefinitionWithoutDocumentation)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Function, FunctionImplementation)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Parser as Core exposing ((|.), (|=))


declaration : Parser State (Node Declaration)
declaration =
    Combine.oneOf
        [ Combine.succeed
            (\documentation ->
                \decl ->
                    case decl.declaration of
                        Declaration.FunctionDeclaration functionDecl ->
                            Combine.succeed
                                (Node { start = (Node.range documentation).start, end = decl.rangeEnd }
                                    (Declaration.FunctionDeclaration { functionDecl | documentation = Just documentation })
                                )

                        Declaration.AliasDeclaration typeAliasDecl ->
                            Combine.succeed
                                (Node { start = (Node.range documentation).start, end = decl.rangeEnd }
                                    (Declaration.AliasDeclaration { typeAliasDecl | documentation = Just documentation })
                                )

                        Declaration.CustomTypeDeclaration customTypeDecl ->
                            Combine.succeed
                                (Node { start = (Node.range documentation).start, end = decl.rangeEnd }
                                    (Declaration.CustomTypeDeclaration { customTypeDecl | documentation = Just documentation })
                                )

                        Declaration.PortDeclaration _ ->
                            Core.map
                                (\( startRow, startColumn ) ->
                                    \sig ->
                                        Node { start = { row = startRow, column = startColumn }, end = (Node.range sig.typeAnnotation).end }
                                            (Declaration.PortDeclaration sig)
                                )
                                Core.getPosition
                                |> Combine.ignoreFromCore (Combine.modifyState (State.addComment documentation))
                                |> Combine.ignoreEntirely Tokens.portToken
                                |> Combine.ignore Layout.layout
                                |> Combine.keep signature

                        Declaration.InfixDeclaration _ ->
                            Combine.succeed (Node { start = (Node.range documentation).start, end = decl.rangeEnd } decl.declaration)

                        Declaration.Destructuring _ _ ->
                            Combine.succeed (Node { start = (Node.range documentation).start, end = decl.rangeEnd } decl.declaration)
            )
            |> Combine.keepFromCore Comments.declarationDocumentation
            |> Combine.ignore Layout.layoutStrict
            |> Combine.keep
                (Combine.oneOf
                    [ functionWithoutDocumentation
                    , typeDefinitionWithoutDocumentation
                    , Combine.succeedLazy
                        (\() ->
                            { rangeEnd = Range.empty.end
                            , declaration =
                                Declaration.PortDeclaration
                                    { name = Node.empty "", typeAnnotation = Node.empty TypeAnnotation.Unit }
                            }
                        )
                    ]
                )
            |> Combine.andThen identity
        , infixDeclaration
        , Combine.fromCoreMap
            (\( rangeStartRow, rangeStartColumn ) ->
                \decl ->
                    Node { start = { row = rangeStartRow, column = rangeStartColumn }, end = decl.rangeEnd }
                        decl.declaration
            )
            Core.getPosition
            |> Combine.keep
                (Combine.oneOf
                    [ functionWithoutDocumentation
                    , typeDefinitionWithoutDocumentation
                    , portDeclarationWithoutDocumentation
                    ]
                )
        ]


functionWithoutDocumentation : Parser State { rangeEnd : Location, declaration : Declaration }
functionWithoutDocumentation =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen functionWithNameNode
        |> Combine.map
            (\f ->
                let
                    functionImplementation : FunctionImplementation
                    functionImplementation =
                        Node.value f.declaration

                    expressionRangeEnd : Location
                    expressionRangeEnd =
                        (Node.range functionImplementation.expression).end
                in
                { rangeEnd = expressionRangeEnd
                , declaration = Declaration.FunctionDeclaration f
                }
            )


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


functionWithSignature : Node String -> Parser State Function
functionWithSignature varPointer =
    functionSignatureFromVarPointer varPointer
        |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)
        |> Combine.andThen
            (\sig ->
                Node.parserFromCore Tokens.functionName
                    |> Combine.andThen (\fnName -> failIfDifferentFrom varPointer fnName)
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    |> Combine.andThen (\body -> functionImplementationFromVarPointer (Just sig) body)
            )


functionWithoutSignature : Node String -> Parser State Function
functionWithoutSignature varPointer =
    functionImplementationFromVarPointer Nothing varPointer


functionImplementationFromVarPointer : Maybe (Node Signature) -> Node String -> Parser State Function
functionImplementationFromVarPointer signature_ ((Node { start } _) as varPointer) =
    Combine.succeed
        (\args ((Node { end } _) as expr) ->
            { documentation = Nothing
            , signature = signature_
            , declaration =
                Node { start = start, end = end }
                    (FunctionImplementation varPointer args expr)
            }
        )
        |> Combine.keep (Combine.many (pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout)))
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep expression


signature : Parser State Signature
signature =
    Combine.succeed (\name -> \typeAnnotation -> { name = name, typeAnnotation = typeAnnotation })
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Layout.maybeAroundBothSides (Combine.symbol ":"))
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    Combine.succeed
        (\direction ->
            \precedence ->
                \operator ->
                    \fn ->
                        Declaration.InfixDeclaration
                            { direction = direction, precedence = precedence, operator = operator, function = fn }
        )
        |> Combine.ignoreEntirely (Core.keyword "infix")
        |> Combine.ignore Layout.layout
        |> Combine.keep infixDirection
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Core.int)
        |> Combine.ignore Layout.layout
        |> Combine.keep operatorWithParens
        |> Combine.ignore Layout.layout
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Node.parser


operatorWithParens : Parser state (Node String)
operatorWithParens =
    Core.succeed identity
        |. Tokens.parensStart
        |= Tokens.prefixOperatorToken
        |. Tokens.parensEnd
        |> Node.parserFromCore


infixDirection : Parser State (Node Infix.InfixDirection)
infixDirection =
    Core.oneOf
        [ Core.keyword "right"
            |> Core.map (\() -> Infix.Right)
        , Core.keyword "left"
            |> Core.map (\() -> Infix.Left)
        , Core.keyword "non"
            |> Core.map (\() -> Infix.Non)
        ]
        |> Node.parserFromCore


portDeclarationWithoutDocumentation : Parser State { rangeEnd : Location, declaration : Declaration }
portDeclarationWithoutDocumentation =
    Combine.succeed
        (\sig ->
            { rangeEnd = (Node.range sig.typeAnnotation).end
            , declaration = Declaration.PortDeclaration sig
            }
        )
        |> Combine.ignoreEntirely Tokens.portToken
        |> Combine.ignore Layout.layout
        |> Combine.keep signature
