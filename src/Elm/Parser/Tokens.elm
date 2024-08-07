module Elm.Parser.Tokens exposing
    ( asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, prefixOperatorToken, thenToken
    , dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
    , minus, minusSymbols
    , characterLiteral, stringLiteral, multiLineStringLiteral
    , functionName, typeName
    )

{-|

@docs asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, prefixOperatorToken, thenToken

@docs dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
@docs minus, minusSymbols

@docs characterLiteral, stringLiteral, multiLineStringLiteral
@docs functionName, typeName

-}

import Char
import Hex
import Parser as Core exposing ((|.), (|=), Step(..))
import Parser.Extra
import Set exposing (Set)
import Unicode


reservedList : Set String
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
        |> Set.fromList


portToken : Core.Parser ()
portToken =
    Core.symbol "port"


moduleToken : Core.Parser ()
moduleToken =
    Core.symbol "module"


whereToken : Core.Parser ()
whereToken =
    Core.symbol "where"


exposingToken : Core.Parser ()
exposingToken =
    Core.symbol "exposing"


importToken : Core.Parser ()
importToken =
    Core.keyword "import"


asToken : Core.Parser ()
asToken =
    Core.keyword "as"


ifToken : Core.Parser ()
ifToken =
    Core.symbol "if"


thenToken : Core.Parser ()
thenToken =
    Core.symbol "then"


elseToken : Core.Parser ()
elseToken =
    Core.symbol "else"


caseToken : Core.Parser ()
caseToken =
    Core.symbol "case"


ofToken : Core.Parser ()
ofToken =
    Core.symbol "of"


letToken : Core.Parser ()
letToken =
    Core.symbol "let"


inToken : Core.Parser ()
inToken =
    Core.symbol "in"


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
        , Core.succeed
            (\hex ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
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
            |= Parser.Extra.anyChar
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
                    , Core.succeed
                        (\start ->
                            \value ->
                                \end ->
                                    if start == end then
                                        Core.problem "Expected a string character or a double quote"

                                    else
                                        Core.succeed (Loop { escaped = s.escaped, parts = value :: s.parts })
                        )
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen identity
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


multiLineStringLiteral : Core.Parser String
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
                        |> Core.mapChompedString (\v () -> Loop { counter = s.counter + 1, escaped = s.escaped, parts = v :: s.parts })
                    , Core.symbol "\\"
                        |> Core.map (\_ -> Loop { counter = s.counter + 1, escaped = True, parts = s.parts })
                    , Core.succeed
                        (\start ->
                            \value ->
                                \end ->
                                    if start == end then
                                        Core.problem "Expected a string character or a triple double quote"

                                    else
                                        Core.succeed (Loop { counter = s.counter + 1, escaped = s.escaped, parts = value :: s.parts })
                        )
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen identity
                    ]
    in
    Core.succeed identity
        |. Core.symbol "\"\"\""
        |= Core.loop { escaped = False, parts = [], counter = 0 } helper


functionName : Core.Parser String
functionName =
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = reservedList
        }


typeName : Core.Parser String
typeName =
    Core.variable
        { start = Unicode.isUpper
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = reservedList
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


minus : Core.Parser ()
minus =
    Core.symbol "-"


minusSymbols : Core.Parser ()
minusSymbols =
    Core.oneOf
        [ Core.symbol "- "
        , Core.symbol "-\n"
        , Core.symbol "-\u{000D}"
        ]


dot : Core.Parser ()
dot =
    Core.symbol "."


dotDot : Core.Parser ()
dotDot =
    Core.symbol ".."


squareStart : Core.Parser ()
squareStart =
    Core.symbol "["


squareEnd : Core.Parser ()
squareEnd =
    Core.symbol "]"


curlyStart : Core.Parser ()
curlyStart =
    Core.symbol "{"


curlyEnd : Core.Parser ()
curlyEnd =
    Core.symbol "}"


pipe : Core.Parser ()
pipe =
    Core.symbol "|"


backSlash : Core.Parser ()
backSlash =
    Core.symbol "\\"


arrowRight : Core.Parser ()
arrowRight =
    Core.symbol "->"


equal : Core.Parser ()
equal =
    Core.symbol "="


comma : Core.Parser ()
comma =
    Core.symbol ","


parensStart : Core.Parser ()
parensStart =
    Core.symbol "("


parensEnd : Core.Parser ()
parensEnd =
    Core.symbol ")"


colon : Core.Parser ()
colon =
    Core.symbol ":"


cons : Core.Parser ()
cons =
    Core.symbol "::"
