module Elm.Syntax.Pattern
    exposing
        ( Pattern
            ( AllPattern
            , AsPattern
            , CharPattern
            , FloatPattern
            , IntPattern
            , ListPattern
            , NamedPattern
            , ParenthesizedPattern
            , QualifiedNamePattern
            , RecordPattern
            , StringPattern
            , TuplePattern
            , UnConsPattern
            , UnitPattern
            , VarPattern
            )
        , QualifiedNameRef
        )

{-| Pattern Syntax


# Types

@docs Pattern, QualifiedNameRef

-}

import Elm.Syntax.Base exposing (VariablePointer)
import Elm.Syntax.Range exposing (Range)


{-| Union type for all the patterns
-}
type Pattern
    = AllPattern Range
    | UnitPattern Range
    | CharPattern Char Range
    | StringPattern String Range
    | IntPattern Int Range
    | FloatPattern Float Range
    | TuplePattern (List Pattern) Range
    | RecordPattern (List VariablePointer) Range
    | UnConsPattern Pattern Pattern Range
    | ListPattern (List Pattern) Range
    | VarPattern String Range
    | NamedPattern QualifiedNameRef (List Pattern) Range
    | QualifiedNamePattern QualifiedNameRef Range
    | AsPattern Pattern VariablePointer Range
    | ParenthesizedPattern Pattern Range


{-| Qualified name reference
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }
