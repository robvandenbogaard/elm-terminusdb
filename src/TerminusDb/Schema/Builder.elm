module TerminusDb.Schema.Builder exposing (..)

import TerminusDb.Woql exposing (..)


doctype : String -> List Query -> Query
doctype d q =
    And <| Triple (Var "a") (Var "b") (Literal d) :: q


label : String -> Query
label l =
    Triple (Var "a") (Var "b") (Literal l)


description : String -> Query
description d =
    Triple (Var "a") (Var "b") (Literal d)


property : String -> Value -> List Query -> Query
property p t q =
    And <| Triple (Var "a") (Literal p) t :: q
