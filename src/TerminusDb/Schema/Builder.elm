module TerminusDb.Schema.Builder exposing (description, doctype, graph, label, property)

import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix exposing (..)
import TerminusDb.Woql exposing (..)


{-| This module exposes query builder functions to define new doctypes.

@docs doctype


# Doctype builder helpers

@docs label, description, property

@docs graph

-}
doctype : String -> List (Value -> Query) -> Query
doctype name queriesForType =
    let
        typeNode =
            Node Scm name
    in
    And
        (AddQuad typeNode (Node Rdf "type") (Node Owl "Class") graph
            :: AddQuad typeNode (Node Rdfs "subClassOf") (Node System "Document") graph
            :: List.map ((|>) typeNode) queriesForType
        )


label : Schema.TranslatedText -> Value -> Query
label l node =
    AddQuad node (Node Rdfs "label") (Datatype l) graph


description : Schema.TranslatedText -> Value -> Query
description d node =
    AddQuad node (Node Rdfs "description") (Datatype d) graph


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


graph =
    Literal "schema/main"
