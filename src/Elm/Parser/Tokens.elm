module Elm.Parser.Tokens exposing
    ( asToken
    , caseToken
    , characterLiteral
    , elseToken
    , exposingToken
    , functionName
    , functionNameCore
    , ifToken
    , importToken
    , inToken
    , letToken
    , moduleToken
    , multiLineStringLiteral
    , ofToken
    , portToken
    , prefixOperatorToken
    , stringLiteral
    , thenToken
    , typeName
    , typeNameCore
    )

import Char
import Combine exposing (Parser, symbol)
import Combine.Char exposing (anyChar)
import Hex
import Parser as Core exposing ((|.), (|=), Step(..))
import Set
import Unicode


reservedList : List String
reservedList =
    [ "module"
    , "exposing"
    , "import"
    , "as"
    , "if"
    , "then"
    , "else"
    , "let"
    , "in"
    , "case"
    , "of"
    , "port"

    --, "infixr"
    --, "infixl"
    , "type"

    --, "infix" Apparently this is not a reserved keyword
    --, "alias" Apparently this is not a reserved keyword
    , "where"
    ]


portToken : Parser state ()
portToken =
    symbol "port"


moduleToken : Parser state ()
moduleToken =
    symbol "module"


exposingToken : Parser state ()
exposingToken =
    symbol "exposing"


importToken : Parser state ()
importToken =
    Combine.fromCore (Core.keyword "import")


asToken : Parser state ()
asToken =
    Combine.fromCore (Core.keyword "as")


ifToken : Parser state ()
ifToken =
    symbol "if"


thenToken : Parser state ()
thenToken =
    symbol "then"


elseToken : Parser state ()
elseToken =
    symbol "else"


caseToken : Parser state ()
caseToken =
    symbol "case"


ofToken : Parser state ()
ofToken =
    symbol "of"


letToken : Parser state ()
letToken =
    symbol "let"


inToken : Parser state ()
inToken =
    symbol "in"


escapedCharValue : Core.Parser Char
escapedCharValue =
    Core.oneOf
        [ Core.succeed '\'' |. Core.symbol "'"
        , Core.succeed '"' |. Core.symbol "\""
        , Core.succeed '\n' |. Core.symbol "n"
        , Core.succeed '\t' |. Core.symbol "t"
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Core.succeed '\u{000D}' |. Core.symbol "r"
        , Core.succeed '\\' |. Core.symbol "\\"
        , Core.succeed (String.toLower >> Hex.fromString >> Result.withDefault 0 >> Char.fromCode)
            |. Core.symbol "u"
            |. Core.symbol "{"
            |= (Core.chompWhile Char.isHexDigit |> Core.getChompedString)
            |. Core.symbol "}"
        ]


quotedSingleQuote : Core.Parser Char
quotedSingleQuote =
    Core.succeed (String.toList >> List.head >> Maybe.withDefault ' ')
        |. Core.symbol "'"
        |= Core.oneOf
            [ Core.succeed String.fromChar
                |. Core.symbol "\\"
                |= escapedCharValue
            , Core.getChompedString (Core.chompIf (always True))
            ]
        |. Core.symbol "'"


characterLiteral : Core.Parser Char
characterLiteral =
    Core.oneOf
        [ quotedSingleQuote
        , Core.succeed identity
            |. Core.symbol "'"
            |= anyChar
            |. Core.symbol "'"
        ]


type alias StringLiteralLoopState =
    { escaped : Bool
    , parts : List String
    }


stringLiteral : Core.Parser String
stringLiteral =
    let
        helper : StringLiteralLoopState -> Core.Parser (Step StringLiteralLoopState String)
        helper s =
            if s.escaped then
                escapedCharValue
                    |> Core.map
                        (\v ->
                            Loop { escaped = False, parts = String.fromChar v :: s.parts }
                        )

            else
                Core.oneOf
                    [ Core.symbol "\"" |> Core.map (\_ -> Done (s.parts |> List.reverse |> String.concat))
                    , Core.symbol "\\" |> Core.map (\_ -> Loop { escaped = True, parts = s.parts })
                    , Core.succeed (\start value end -> ( start, value, end ))
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Core.problem "Expected a string character or a double quote"

                                else
                                    Core.succeed (Loop { escaped = s.escaped, parts = value :: s.parts })
                            )
                    ]
    in
    Core.succeed identity
        |. Core.symbol "\""
        |= Core.loop { escaped = False, parts = [] } helper


type alias MultilineStringLiteralLoopState =
    { escaped : Bool
    , parts : List String
    , counter : Int
    }


multiLineStringLiteral : Parser state String
multiLineStringLiteral =
    let
        helper : MultilineStringLiteralLoopState -> Core.Parser (Step MultilineStringLiteralLoopState String)
        helper s =
            if s.escaped then
                escapedCharValue
                    |> Core.map (\v -> Loop { counter = s.counter, escaped = False, parts = String.fromChar v :: s.parts })

            else
                Core.oneOf
                    [ Core.symbol "\"\"\""
                        |> Core.map (\_ -> Done (String.concat (List.reverse s.parts)))
                    , Core.symbol "\""
                        |> Core.mapChompedString (\v _ -> Loop { counter = s.counter + 1, escaped = s.escaped, parts = v :: s.parts })
                    , Core.symbol "\\"
                        |> Core.map (\_ -> Loop { counter = s.counter + 1, escaped = True, parts = s.parts })
                    , Core.succeed (\start value end -> ( start, value, end ))
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Core.problem "Expected a string character or a triple double quote"

                                else
                                    Core.succeed (Loop { counter = s.counter + 1, escaped = s.escaped, parts = value :: s.parts })
                            )
                    ]
    in
    Core.succeed identity
        |. Core.symbol "\"\"\""
        |= Core.loop { escaped = False, parts = [], counter = 0 } helper
        |> Combine.fromCore


functionName : Parser state String
functionName =
    Combine.fromCore functionNameCore


functionNameCore : Core.Parser String
functionNameCore =
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedList
        }


typeName : Parser state String
typeName =
    Combine.fromCore typeNameCore


typeNameCore : Core.Parser String
typeNameCore =
    Core.variable
        { start = Unicode.isUpper
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedList
        }


allowedOperatorTokens : List String
allowedOperatorTokens =
    [ "=="
    , "/="
    , "::"
    , "++"
    , "+"
    , "*"
    , "<|"
    , "|>"
    , "||"
    , "<="
    , ">="
    , "|="
    , "|."
    , "//"
    , "</>"
    , "<?>"
    , "^"
    , "<<"
    , ">>"
    , "<"
    , ">"
    , "/"
    , "&&"
    , "-"
    ]


prefixOperatorToken : Core.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> Core.token token |> Core.getChompedString)
        |> Core.oneOf
