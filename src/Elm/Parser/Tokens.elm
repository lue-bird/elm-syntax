module Elm.Parser.Tokens exposing
    ( inToken
    , squareEnd, curlyEnd, arrowRight, equal, parensEnd
    , minusFollowedBySingleWhitespace
    , prefixOperatorToken, allowedOperatorTokens
    , characterLiteral, singleOrTripleQuotedStringLiteral
    , functionName, functionNameNotInfix, typeName
    )

{-|

@docs inToken

@docs squareEnd, curlyEnd, arrowRight, equal, parensEnd
@docs minusFollowedBySingleWhitespace
@docs prefixOperatorToken, allowedOperatorTokens

@docs characterLiteral, singleOrTripleQuotedStringLiteral
@docs functionName, functionNameNotInfix, typeName

-}

import Char
import Char.Extra
import Hex
import ParserFast
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


inToken : ParserFast.Parser ()
inToken =
    ParserFast.keyword "in" ()


escapedCharValueMap : (Char -> res) -> ParserFast.Parser res
escapedCharValueMap charToRes =
    ParserFast.oneOf
        [ ParserFast.symbol1 '\'' (charToRes '\'')
        , ParserFast.symbol1 '"' (charToRes '"')
        , ParserFast.symbol1 'n' (charToRes '\n')
        , ParserFast.symbol1 't' (charToRes '\t')
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          ParserFast.symbol1 'r' (charToRes '\u{000D}')
        , ParserFast.symbol1 '\\' (charToRes '\\')
        , ParserFast.map2
            (\hex () ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        charToRes (Char.fromCode n)

                    Err _ ->
                        charToRes '\u{0000}'
            )
            (ParserFast.symbol2FollowedBy 'u'
                '{'
                (ParserFast.ifFollowedByWhile
                    Char.isHexDigit
                    Char.isHexDigit
                )
            )
            (ParserFast.symbol1 '}' ())
        ]


slashEscapedCharValue : ParserFast.Parser Char
slashEscapedCharValue =
    ParserFast.symbol1FollowedBy '\\' (escapedCharValueMap identity)


characterLiteral : ParserFast.Parser Char
characterLiteral =
    ParserFast.map2
        (\res () -> res)
        (ParserFast.symbol1FollowedBy '\''
            (ParserFast.oneOf2
                slashEscapedCharValue
                ParserFast.anyChar
            )
        )
        (ParserFast.symbol1 '\'' ())


singleOrTripleQuotedStringLiteral : ParserFast.Parser String
singleOrTripleQuotedStringLiteral =
    ParserFast.symbol1FollowedBy '"'
        (ParserFast.oneOf2
            (ParserFast.symbol2FollowedBy '"'
                '"'
                tripleQuotedStringLiteralOfterTripleDoubleQuote
            )
            singleQuotedStringLiteralAfterDoubleQuote
        )


singleQuotedStringLiteralAfterDoubleQuote : ParserFast.Parser String
singleQuotedStringLiteralAfterDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol1 '"' ())
        (ParserFast.oneOf2
            (ParserFast.symbol1FollowedBy '\\' (escapedCharValueMap String.fromChar))
            (ParserFast.whileMap (\c -> c /= '"' && c /= '\\') identity)
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


tripleQuotedStringLiteralOfterTripleDoubleQuote : ParserFast.Parser String
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol3 '"' '"' '"' ())
        (ParserFast.oneOf3
            (ParserFast.symbol1 '"' "\"")
            (ParserFast.symbol1FollowedBy '\\' (escapedCharValueMap String.fromChar))
            (ParserFast.whileMap (\c -> c /= '"' && c /= '\\') identity)
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


functionName : ParserFast.Parser String
functionName =
    ParserFast.ifFollowedByWhileExcept
        (\c -> Char.isLower c || Unicode.isLower c)
        (\c ->
            -- checking for these common ranges early is much faster
            Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        )
        reservedList


functionNameNotInfix : ParserFast.Parser String
functionNameNotInfix =
    ParserFast.ifFollowedByWhileExcept
        (\c -> Char.isLower c || Unicode.isLower c)
        (\c ->
            -- checking for these common ranges early is much faster
            Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        )
        (Set.insert "infix" reservedList)


typeName : ParserFast.Parser String
typeName =
    ParserFast.ifFollowedByWhile
        (\c -> Char.isUpper c || Unicode.isUpper c)
        (\c ->
            -- checking for these common ranges early is much faster
            Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        )


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


prefixOperatorToken : ParserFast.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> ParserFast.symbol token token)
        |> ParserFast.oneOf


minusFollowedBySingleWhitespace : ParserFast.Parser res -> ParserFast.Parser res
minusFollowedBySingleWhitespace next =
    ParserFast.oneOf3
        (ParserFast.symbol2FollowedBy '-' ' ' next)
        (ParserFast.symbol2FollowedBy '-' '\n' next)
        (ParserFast.symbol2FollowedBy '-' '\u{000D}' next)


squareEnd : ParserFast.Parser ()
squareEnd =
    ParserFast.symbol1 ']' ()


curlyEnd : ParserFast.Parser ()
curlyEnd =
    ParserFast.symbol1 '}' ()


arrowRight : ParserFast.Parser ()
arrowRight =
    ParserFast.symbol2 '-' '>' ()


equal : ParserFast.Parser ()
equal =
    ParserFast.symbol1 '=' ()


parensEnd : ParserFast.Parser ()
parensEnd =
    ParserFast.symbol1 ')' ()
