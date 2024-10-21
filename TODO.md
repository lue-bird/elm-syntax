introduce -optimisticLayout version of typeAnnotationNoFnExcludingTypedWithArguments
(and avoid other possible duplicate layout checks)




rename expression to expressionOptimisticLayout
and use it as such (replacing maybeLayout after expression with Layout.positivelyIndented)


rename Elm.Parser.* modules to singular


fix pipes using symbolFollowedBy symbol (problem "cannot mix (" ++ symbol ++ ") and (" ++ symbol ++ ") without parentheses.")


attempt converting module names etc to loop/loopWhileStackUnsafe (TODO introduce)


eliminate mapWithRange (?)


only parse function call if possible (?)



tried and failed:
maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.loopWhileSucceedsRightToLeftStackUnsafe
        (ParserFast.symbolFollowedBy "." Tokens.typeName)
        Nothing
        (\head tail ->
            Just
                (case tail of
                    Nothing ->
                        ( [], head )

                    Just ( tailBeforeLast, last ) ->
                        ( head :: tailBeforeLast, last )
                )
        )





I'm seeing bugs regarding right associativity in the pratt parser.
E.g. `identity <| identity |> identity`
throws the elm compiler error
```md
You cannot mix (<|) and (|>) without parentheses.

1|   identity <| identity |> identity
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
I do not know how to group these expressions. Add parentheses for me!
```
but parses fine
```elm
ParserFast.run (ParserFast.withIndent 0 Elm.Parser.Expression.expression) "identity <| identity |> identity"
--> Ok ...
```
(same with e.g. `<<` into `>>`)

To be honest, my goal is is mostly making the code clearer by avoiding recursion & lazy, and faster by skipping checks for unnecessary operators.
Making the parser more strict is mostly a nice benefit.

Do you think replacing e.g. `<| ..same precedence or higher..`
by `{ <| sub-expression }+ ..precedence+1 or higher..` (curlies meaning repeat)
is a correct alternative?


----- counts


{
  "expressionCount": {
    "FunctionOrValue": 650782,
    "Application": 277573,
    "Literal": 118422,
    "NumberExpression": 107337,
    "ParenthesizedExpression": 75580,
    "ListExpr": 58441,
    "TupledExpression": 32216,
    "RecordExpr": 30250,
    "CaseExpression": 19130,
    "LambdaExpression": 14475,
    "LetExpression": 10226,
    "RecordUpdateExpression": 9067,
    "IfBlock": 7000,
    "RecordAccessFunction": 3416,
    "UnitExpr": 1050,
    "CharLiteral": 668,
    "GLSLExpression": 16,
    "RecordAccess": 71020,
    "Negation": 3192
  },
  "operatorCount": {
    "|>": 56575,
    "++": 8867,
    "<|": 7320,
    ">>": 3350,
    "==": 2902,
    "*": 2604,
    "::": 2107,
    "+": 1748,
    "-": 1588,
    "|.": 1022,
    "&&": 996,
    "|=": 876,
    "<<": 682,
    "/=": 657,
    "/": 593,
    "</>": 580,
    "||": 519,
    ">": 394,
    "<": 325,
    "<=": 237,
    ">=": 232,
    "//": 158,
    "^": 117,
    "<?>": 18
  },
  "patternCount": {
    "VarPattern": 103468,
    "NamedPattern": 77877,
    "AllPattern": 17206,
    "ParenthesizedPattern": 7552,
    "TuplePattern": 5864,
    "RecordPattern": 4649,
    "StringPattern": 4140,
    "UnitPattern": 3637,
    "ListPattern": 1068,
    "UnConsPattern": 835,
    "NumbePattern": 531,
    "AsPattern": 314,
    "CharPattern": 18
  },
  "prefixOperatorCount": {
    "==": 185,
    "/=": 50,
    "::": 46,
    "+": 32,
    "*": 24,
    "++": 7,
    ">": 4,
    "||": 3,
    "|>": 3,
    "<|": 3,
    "-": 3,
    ">=": 2,
    "<": 2,
    "^": 1,
    ">>": 1,
    "<=": 1,
    "/": 1,
    "&&": 1
  }
}

----- zombie


oneOf8 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf8 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7 ])
        )


oneOf9 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf9 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8 ])
        )


oneOf11 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf11 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) (Parser attempt10) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    case attempt10 s of
                                                                                                                                        (Good _ _) as good ->
                                                                                                                                            good

                                                                                                                                        (Bad committed10 x10) as bad10 ->
                                                                                                                                            if committed10 then
                                                                                                                                                bad10

                                                                                                                                            else
                                                                                                                                                Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8, x9, x10 ])
        )


oneOf14 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf14 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) (Parser attempt10) (Parser attempt11) (Parser attempt12) (Parser attempt13) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    case attempt10 s of
                                                                                                                                        (Good _ _) as good ->
                                                                                                                                            good

                                                                                                                                        (Bad committed10 x10) as bad10 ->
                                                                                                                                            if committed10 then
                                                                                                                                                bad10

                                                                                                                                            else
                                                                                                                                                case attempt11 s of
                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                        good

                                                                                                                                                    (Bad committed11 x11) as bad11 ->
                                                                                                                                                        if committed11 then
                                                                                                                                                            bad11

                                                                                                                                                        else
                                                                                                                                                            case attempt12 s of
                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                    good

                                                                                                                                                                (Bad committed12 x12) as bad12 ->
                                                                                                                                                                    if committed12 then
                                                                                                                                                                        bad12

                                                                                                                                                                    else
                                                                                                                                                                        case attempt13 s of
                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                good

                                                                                                                                                                            (Bad committed13 x13) as bad13 ->
                                                                                                                                                                                if committed13 then
                                                                                                                                                                                    bad13

                                                                                                                                                                                else
                                                                                                                                                                                    Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13 ])
        )


oneOf20 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf20 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) (Parser attempt10) (Parser attempt11) (Parser attempt12) (Parser attempt13) (Parser attempt14) (Parser attempt15) (Parser attempt16) (Parser attempt17) (Parser attempt18) (Parser attempt19) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    case attempt10 s of
                                                                                                                                        (Good _ _) as good ->
                                                                                                                                            good

                                                                                                                                        (Bad committed10 x10) as bad10 ->
                                                                                                                                            if committed10 then
                                                                                                                                                bad10

                                                                                                                                            else
                                                                                                                                                case attempt11 s of
                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                        good

                                                                                                                                                    (Bad committed11 x11) as bad11 ->
                                                                                                                                                        if committed11 then
                                                                                                                                                            bad11

                                                                                                                                                        else
                                                                                                                                                            case attempt12 s of
                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                    good

                                                                                                                                                                (Bad committed12 x12) as bad12 ->
                                                                                                                                                                    if committed12 then
                                                                                                                                                                        bad12

                                                                                                                                                                    else
                                                                                                                                                                        case attempt13 s of
                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                good

                                                                                                                                                                            (Bad committed13 x13) as bad13 ->
                                                                                                                                                                                if committed13 then
                                                                                                                                                                                    bad13

                                                                                                                                                                                else
                                                                                                                                                                                    case attempt14 s of
                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                            good

                                                                                                                                                                                        (Bad committed14 x14) as bad14 ->
                                                                                                                                                                                            if committed14 then
                                                                                                                                                                                                bad14

                                                                                                                                                                                            else
                                                                                                                                                                                                case attempt15 s of
                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                        good

                                                                                                                                                                                                    (Bad committed15 x15) as bad15 ->
                                                                                                                                                                                                        if committed15 then
                                                                                                                                                                                                            bad15

                                                                                                                                                                                                        else
                                                                                                                                                                                                            case attempt16 s of
                                                                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                                                                    good

                                                                                                                                                                                                                (Bad committed16 x16) as bad16 ->
                                                                                                                                                                                                                    if committed16 then
                                                                                                                                                                                                                        bad16

                                                                                                                                                                                                                    else
                                                                                                                                                                                                                        case attempt17 s of
                                                                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                                                                good

                                                                                                                                                                                                                            (Bad committed17 x17) as bad17 ->
                                                                                                                                                                                                                                if committed17 then
                                                                                                                                                                                                                                    bad17

                                                                                                                                                                                                                                else
                                                                                                                                                                                                                                    case attempt18 s of
                                                                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                                                                            good

                                                                                                                                                                                                                                        (Bad committed18 x18) as bad18 ->
                                                                                                                                                                                                                                            if committed18 then
                                                                                                                                                                                                                                                bad18

                                                                                                                                                                                                                                            else
                                                                                                                                                                                                                                                case attempt19 s of
                                                                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                                                                        good

                                                                                                                                                                                                                                                    (Bad committed19 x19) as bad19 ->
                                                                                                                                                                                                                                                        if committed19 then
                                                                                                                                                                                                                                                            bad19

                                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                                            Bad False
                                                                                                                                                                                                                                                                (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19 ])
        )


oneOf21 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf21 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) (Parser attempt10) (Parser attempt11) (Parser attempt12) (Parser attempt13) (Parser attempt14) (Parser attempt15) (Parser attempt16) (Parser attempt17) (Parser attempt18) (Parser attempt19) (Parser attempt20) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    case attempt10 s of
                                                                                                                                        (Good _ _) as good ->
                                                                                                                                            good

                                                                                                                                        (Bad committed10 x10) as bad10 ->
                                                                                                                                            if committed10 then
                                                                                                                                                bad10

                                                                                                                                            else
                                                                                                                                                case attempt11 s of
                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                        good

                                                                                                                                                    (Bad committed11 x11) as bad11 ->
                                                                                                                                                        if committed11 then
                                                                                                                                                            bad11

                                                                                                                                                        else
                                                                                                                                                            case attempt12 s of
                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                    good

                                                                                                                                                                (Bad committed12 x12) as bad12 ->
                                                                                                                                                                    if committed12 then
                                                                                                                                                                        bad12

                                                                                                                                                                    else
                                                                                                                                                                        case attempt13 s of
                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                good

                                                                                                                                                                            (Bad committed13 x13) as bad13 ->
                                                                                                                                                                                if committed13 then
                                                                                                                                                                                    bad13

                                                                                                                                                                                else
                                                                                                                                                                                    case attempt14 s of
                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                            good

                                                                                                                                                                                        (Bad committed14 x14) as bad14 ->
                                                                                                                                                                                            if committed14 then
                                                                                                                                                                                                bad14

                                                                                                                                                                                            else
                                                                                                                                                                                                case attempt15 s of
                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                        good

                                                                                                                                                                                                    (Bad committed15 x15) as bad15 ->
                                                                                                                                                                                                        if committed15 then
                                                                                                                                                                                                            bad15

                                                                                                                                                                                                        else
                                                                                                                                                                                                            case attempt16 s of
                                                                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                                                                    good

                                                                                                                                                                                                                (Bad committed16 x16) as bad16 ->
                                                                                                                                                                                                                    if committed16 then
                                                                                                                                                                                                                        bad16

                                                                                                                                                                                                                    else
                                                                                                                                                                                                                        case attempt17 s of
                                                                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                                                                good

                                                                                                                                                                                                                            (Bad committed17 x17) as bad17 ->
                                                                                                                                                                                                                                if committed17 then
                                                                                                                                                                                                                                    bad17

                                                                                                                                                                                                                                else
                                                                                                                                                                                                                                    case attempt18 s of
                                                                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                                                                            good

                                                                                                                                                                                                                                        (Bad committed18 x18) as bad18 ->
                                                                                                                                                                                                                                            if committed18 then
                                                                                                                                                                                                                                                bad18

                                                                                                                                                                                                                                            else
                                                                                                                                                                                                                                                case attempt19 s of
                                                                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                                                                        good

                                                                                                                                                                                                                                                    (Bad committed19 x19) as bad19 ->
                                                                                                                                                                                                                                                        if committed19 then
                                                                                                                                                                                                                                                            bad19

                                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                                            case attempt20 s of
                                                                                                                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                                                                                                                    good

                                                                                                                                                                                                                                                                (Bad committed20 x20) as bad20 ->
                                                                                                                                                                                                                                                                    if committed20 then
                                                                                                                                                                                                                                                                        bad20

                                                                                                                                                                                                                                                                    else
                                                                                                                                                                                                                                                                        Bad False
                                                                                                                                                                                                                                                                            (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20 ])
        )


oneOf22 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf22 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) (Parser attempt10) (Parser attempt11) (Parser attempt12) (Parser attempt13) (Parser attempt14) (Parser attempt15) (Parser attempt16) (Parser attempt17) (Parser attempt18) (Parser attempt19) (Parser attempt20) (Parser attempt21) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    case attempt10 s of
                                                                                                                                        (Good _ _) as good ->
                                                                                                                                            good

                                                                                                                                        (Bad committed10 x10) as bad10 ->
                                                                                                                                            if committed10 then
                                                                                                                                                bad10

                                                                                                                                            else
                                                                                                                                                case attempt11 s of
                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                        good

                                                                                                                                                    (Bad committed11 x11) as bad11 ->
                                                                                                                                                        if committed11 then
                                                                                                                                                            bad11

                                                                                                                                                        else
                                                                                                                                                            case attempt12 s of
                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                    good

                                                                                                                                                                (Bad committed12 x12) as bad12 ->
                                                                                                                                                                    if committed12 then
                                                                                                                                                                        bad12

                                                                                                                                                                    else
                                                                                                                                                                        case attempt13 s of
                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                good

                                                                                                                                                                            (Bad committed13 x13) as bad13 ->
                                                                                                                                                                                if committed13 then
                                                                                                                                                                                    bad13

                                                                                                                                                                                else
                                                                                                                                                                                    case attempt14 s of
                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                            good

                                                                                                                                                                                        (Bad committed14 x14) as bad14 ->
                                                                                                                                                                                            if committed14 then
                                                                                                                                                                                                bad14

                                                                                                                                                                                            else
                                                                                                                                                                                                case attempt15 s of
                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                        good

                                                                                                                                                                                                    (Bad committed15 x15) as bad15 ->
                                                                                                                                                                                                        if committed15 then
                                                                                                                                                                                                            bad15

                                                                                                                                                                                                        else
                                                                                                                                                                                                            case attempt16 s of
                                                                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                                                                    good

                                                                                                                                                                                                                (Bad committed16 x16) as bad16 ->
                                                                                                                                                                                                                    if committed16 then
                                                                                                                                                                                                                        bad16

                                                                                                                                                                                                                    else
                                                                                                                                                                                                                        case attempt17 s of
                                                                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                                                                good

                                                                                                                                                                                                                            (Bad committed17 x17) as bad17 ->
                                                                                                                                                                                                                                if committed17 then
                                                                                                                                                                                                                                    bad17

                                                                                                                                                                                                                                else
                                                                                                                                                                                                                                    case attempt18 s of
                                                                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                                                                            good

                                                                                                                                                                                                                                        (Bad committed18 x18) as bad18 ->
                                                                                                                                                                                                                                            if committed18 then
                                                                                                                                                                                                                                                bad18

                                                                                                                                                                                                                                            else
                                                                                                                                                                                                                                                case attempt19 s of
                                                                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                                                                        good

                                                                                                                                                                                                                                                    (Bad committed19 x19) as bad19 ->
                                                                                                                                                                                                                                                        if committed19 then
                                                                                                                                                                                                                                                            bad19

                                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                                            case attempt20 s of
                                                                                                                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                                                                                                                    good

                                                                                                                                                                                                                                                                (Bad committed20 x20) as bad20 ->
                                                                                                                                                                                                                                                                    if committed20 then
                                                                                                                                                                                                                                                                        bad20

                                                                                                                                                                                                                                                                    else
                                                                                                                                                                                                                                                                        case attempt21 s of
                                                                                                                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                                                                                                                good

                                                                                                                                                                                                                                                                            (Bad committed21 x21) as bad21 ->
                                                                                                                                                                                                                                                                                if committed21 then
                                                                                                                                                                                                                                                                                    bad21

                                                                                                                                                                                                                                                                                else
                                                                                                                                                                                                                                                                                    Bad False
                                                                                                                                                                                                                                                                                        (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21 ])
        )


oneOf24 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf24 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) (Parser attempt10) (Parser attempt11) (Parser attempt12) (Parser attempt13) (Parser attempt14) (Parser attempt15) (Parser attempt16) (Parser attempt17) (Parser attempt18) (Parser attempt19) (Parser attempt20) (Parser attempt21) (Parser attempt22) (Parser attempt23) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    case attempt10 s of
                                                                                                                                        (Good _ _) as good ->
                                                                                                                                            good

                                                                                                                                        (Bad committed10 x10) as bad10 ->
                                                                                                                                            if committed10 then
                                                                                                                                                bad10

                                                                                                                                            else
                                                                                                                                                case attempt11 s of
                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                        good

                                                                                                                                                    (Bad committed11 x11) as bad11 ->
                                                                                                                                                        if committed11 then
                                                                                                                                                            bad11

                                                                                                                                                        else
                                                                                                                                                            case attempt12 s of
                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                    good

                                                                                                                                                                (Bad committed12 x12) as bad12 ->
                                                                                                                                                                    if committed12 then
                                                                                                                                                                        bad12

                                                                                                                                                                    else
                                                                                                                                                                        case attempt13 s of
                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                good

                                                                                                                                                                            (Bad committed13 x13) as bad13 ->
                                                                                                                                                                                if committed13 then
                                                                                                                                                                                    bad13

                                                                                                                                                                                else
                                                                                                                                                                                    case attempt14 s of
                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                            good

                                                                                                                                                                                        (Bad committed14 x14) as bad14 ->
                                                                                                                                                                                            if committed14 then
                                                                                                                                                                                                bad14

                                                                                                                                                                                            else
                                                                                                                                                                                                case attempt15 s of
                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                        good

                                                                                                                                                                                                    (Bad committed15 x15) as bad15 ->
                                                                                                                                                                                                        if committed15 then
                                                                                                                                                                                                            bad15

                                                                                                                                                                                                        else
                                                                                                                                                                                                            case attempt16 s of
                                                                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                                                                    good

                                                                                                                                                                                                                (Bad committed16 x16) as bad16 ->
                                                                                                                                                                                                                    if committed16 then
                                                                                                                                                                                                                        bad16

                                                                                                                                                                                                                    else
                                                                                                                                                                                                                        case attempt17 s of
                                                                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                                                                good

                                                                                                                                                                                                                            (Bad committed17 x17) as bad17 ->
                                                                                                                                                                                                                                if committed17 then
                                                                                                                                                                                                                                    bad17

                                                                                                                                                                                                                                else
                                                                                                                                                                                                                                    case attempt18 s of
                                                                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                                                                            good

                                                                                                                                                                                                                                        (Bad committed18 x18) as bad18 ->
                                                                                                                                                                                                                                            if committed18 then
                                                                                                                                                                                                                                                bad18

                                                                                                                                                                                                                                            else
                                                                                                                                                                                                                                                case attempt19 s of
                                                                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                                                                        good

                                                                                                                                                                                                                                                    (Bad committed19 x19) as bad19 ->
                                                                                                                                                                                                                                                        if committed19 then
                                                                                                                                                                                                                                                            bad19

                                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                                            case attempt20 s of
                                                                                                                                                                                                                                                                (Good _ _) as good ->
                                                                                                                                                                                                                                                                    good

                                                                                                                                                                                                                                                                (Bad committed20 x20) as bad20 ->
                                                                                                                                                                                                                                                                    if committed20 then
                                                                                                                                                                                                                                                                        bad20

                                                                                                                                                                                                                                                                    else
                                                                                                                                                                                                                                                                        case attempt21 s of
                                                                                                                                                                                                                                                                            (Good _ _) as good ->
                                                                                                                                                                                                                                                                                good

                                                                                                                                                                                                                                                                            (Bad committed21 x21) as bad21 ->
                                                                                                                                                                                                                                                                                if committed21 then
                                                                                                                                                                                                                                                                                    bad21

                                                                                                                                                                                                                                                                                else
                                                                                                                                                                                                                                                                                    case attempt22 s of
                                                                                                                                                                                                                                                                                        (Good _ _) as good ->
                                                                                                                                                                                                                                                                                            good

                                                                                                                                                                                                                                                                                        (Bad committed22 x22) as bad22 ->
                                                                                                                                                                                                                                                                                            if committed22 then
                                                                                                                                                                                                                                                                                                bad22

                                                                                                                                                                                                                                                                                            else
                                                                                                                                                                                                                                                                                                case attempt23 s of
                                                                                                                                                                                                                                                                                                    (Good _ _) as good ->
                                                                                                                                                                                                                                                                                                        good

                                                                                                                                                                                                                                                                                                    (Bad committed23 x23) as bad23 ->
                                                                                                                                                                                                                                                                                                        if committed23 then
                                                                                                                                                                                                                                                                                                            bad23

                                                                                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                                                                                            Bad False
                                                                                                                                                                                                                                                                                                                (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23 ])
        )

{-| Try a dynamic or large list of possibilities where existing oneOfN helpers aren't enough.
-}
oneOf : List (Parser a) -> Parser a
oneOf possibilities =
    case possibilities of
        [] ->
            Parser (\s -> Bad False (ExpectingNonEmptyOneOf s.row s.col))

        [ onlyPossibility ] ->
            onlyPossibility

        (Parser parseFirst) :: (Parser parseSecond) :: remainingParsers ->
            Parser
                (\s ->
                    case parseFirst s of
                        (Good _ _) as good ->
                            good

                        (Bad firstCommitted firstX) as firstBad ->
                            if firstCommitted then
                                firstBad

                            else
                                case parseSecond s of
                                    (Good _ _) as good ->
                                        good

                                    (Bad secondCommitted secondX) as secondBad ->
                                        if secondCommitted then
                                            secondBad

                                        else
                                            oneOfHelp s firstX secondX [] remainingParsers
                )


oneOfHelp : State -> Problem -> Problem -> List Problem -> List (Parser a) -> PStep a
oneOfHelp s0 firstX secondX remainingProblemsSoFar parsers =
    case parsers of
        [] ->
            Bad False (ExpectingOneOf firstX secondX remainingProblemsSoFar)

        (Parser parse) :: remainingParsers ->
            case parse s0 of
                (Good _ _) as good ->
                    good

                (Bad committed x) as bad ->
                    if committed then
                        bad

                    else
                        oneOfHelp s0 firstX secondX (x :: remainingProblemsSoFar) remainingParsers


oneOf10 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf10 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8, x9 ])
        )

map7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser value
map7 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad c2 x ->
                            Bad c2 x

                        Good b s2 ->
                            case parseC s2 of
                                Bad c3 x ->
                                    Bad c3 x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad c4 x ->
                                            Bad c4 x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad c5 x ->
                                                    Bad c5 x

                                                Good e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x ->
                                                            Bad c6 x

                                                        Good f s6 ->
                                                            case parseG s6 of
                                                                Bad c7 x ->
                                                                    Bad c7 x

                                                                Good g s7 ->
                                                                    Good (func a b c d e f g) s7
        )

offsetAsDecimal : Int -> { offset : Int, base : Base }
offsetAsDecimal offset =
    { base = Decimal, offset = offset }


skipIntegerDecimalOrHexadecimal : Int -> String -> { offset : Int, base : Base }
skipIntegerDecimalOrHexadecimal offset src =
    case String.slice offset (offset + 1) src of
        "0" ->
            case String.slice (offset + 1) (offset + 2) src of
                "x" ->
                    { base = Hexadecimal, offset = skip1OrMoreHexadecimal (offset + 2) src }

                _ ->
                    { base = Decimal, offset = offset + 1 }

        "1" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "2" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "3" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "4" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "5" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "6" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "7" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "8" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        "9" ->
            offsetAsDecimal (skip0OrMoreDigits0To9 (offset + 1) src)

        _ ->
            { offset = -1, base = Decimal }


skip1OrMoreHexadecimal : Int -> String -> Int
skip1OrMoreHexadecimal offset src =
    case String.slice offset (offset + 1) src of
        "0" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "1" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "2" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "3" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "4" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "5" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "6" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "7" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "8" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "9" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "a" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "A" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "b" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "B" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "c" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "C" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "d" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "D" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "e" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "E" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "f" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "F" ->
            skip0OrMoreHexadecimal (offset + 1) src

        _ ->
            -1


skip0OrMoreHexadecimal : Int -> String -> Int
skip0OrMoreHexadecimal offset src =
    case String.slice offset (offset + 1) src of
        "0" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "1" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "2" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "3" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "4" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "5" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "6" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "7" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "8" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "9" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "a" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "A" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "b" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "B" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "c" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "C" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "d" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "D" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "e" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "E" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "f" ->
            skip0OrMoreHexadecimal (offset + 1) src

        "F" ->
            skip0OrMoreHexadecimal (offset + 1) src

        _ ->
            offset

