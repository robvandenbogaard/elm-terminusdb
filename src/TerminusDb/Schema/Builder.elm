module TerminusDb.Schema.Builder exposing
    ( doctype
    , label, description, property
    , graph
    )

{-| This module exposes query builder functions to define new doctypes.

@docs doctype


# Doctype builder helpers

@docs label, description, property

@docs graph

-}

import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix exposing (..)
import TerminusDb.Woql exposing (..)


{-| Helper for building a doctype specification query. Takes the target document
type name and parent document class, with a list of queries for further
specification of the doctype, carrying a parameter for passing the doctype
reference.
-}
doctype : String -> Value -> List (Value -> Query) -> Query
doctype name parent queriesForType =
    let
        typeNode =
            Node Scm name
    in
    And
        (AddQuad typeNode (Node Rdf "type") (Node Owl "Class") graph
            :: AddQuad typeNode (Node Rdfs "subClassOf") parent graph
            :: List.map ((|>) typeNode) queriesForType
        )


{-| Helper for adding a label property/field to a doctype. Takes a translated
field description and a reference to the target doctype.
-}
label : Schema.TranslatedText -> Value -> Query
label l node =
    AddQuad node (Node Rdfs "label") (Datatype l) graph


{-| Helper for adding a description property/field to a doctype. Takes a
translated field description and the a reference to the target doctype.
-}
description : Schema.TranslatedText -> Value -> Query
description d node =
    AddQuad node (Node Rdfs "description") (Datatype d) graph


{-| Helper for adding a custom property/field to a doctype. Takes the target
property name and range, and sets the domain to the specified target doctype.
Applies further specification of the property via the provided queries.
-}
property : String -> Value -> List (Value -> Query) -> Value -> Query
property name propertyType queriesForProperty node =
    let
        propNode =
            Node Scm name
    in
    And
        (AddQuad propNode (Node Rdf "type") (Node Woql "DatatypeProperty") graph
            :: AddQuad propNode (Node Rdfs "range") propertyType graph
            :: AddQuad propNode (Node Rdfs "domain") node graph
            :: List.map ((|>) propNode) queriesForProperty
        )


{-| Helper providing a literal referring to the default graph.
-}
graph : Value
graph =
    Literal "schema/main"
