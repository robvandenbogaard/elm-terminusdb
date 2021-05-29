module TerminusDb.Schema.Prefix exposing
    ( Prefix(..), Context
    , context, fromContext
    , encodeContext, decodeContext
    , string, uri, uriFromContext
    )

{-| This module provides preset schema prefixes and helpers for handling prefix
contexts.

@docs Prefix, Context

@doc string, uri

@docs context, fromContext

@docs encodeContext, decodeContext

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Context =
    Dict String String


type Prefix
    = Unprefixed
    | Api
    | Doc
    | Owl
    | Rdf
    | Rdfs
    | Scm
    | System
    | Woql
    | Xsd


{-| Assemble a context from a list of prefixes.
-}
context : List Prefix -> Context
context =
    List.foldl
        (\p a -> Dict.insert (string p) (uri p) a)
        Dict.empty


{-| Return the Prefix as a String.
-}
string : Prefix -> String
string prefix =
    case prefix of
        Unprefixed ->
            ""

        Api ->
            "api"

        Doc ->
            "doc"

        Owl ->
            "owl"

        Rdf ->
            "rdf"

        Rdfs ->
            "rdfs"

        Scm ->
            "scm"

        System ->
            "system"

        Woql ->
            "woql"

        Xsd ->
            "xsd"


decodeContext : Decoder Context
decodeContext =
    Decode.field "@context" (Decode.dict Decode.string)
        |> Decode.maybe
        |> Decode.andThen
            (\c ->
                Decode.succeed (Dict.insert "api" (uri Api) (Maybe.withDefault Dict.empty c))
            )


encodeContext : Context -> Encode.Value
encodeContext =
    Encode.dict identity Encode.string


{-| Return the associated uri for a Prefix.
-}
uri : Prefix -> String
uri prefix =
    case prefix of
        Unprefixed ->
            ""

        Api ->
            "http://terminusdb.com/schema/api"

        Doc ->
            "terminusdb:///data/"

        Owl ->
            "http://www.w3.org/2002/07/owl#"

        Rdf ->
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

        Rdfs ->
            "http://www.w3.org/2000/01/rdf-schema#"

        Scm ->
            "terminusdb:///schema#"

        System ->
            "http://terminusdb.com/schema/system#"

        Woql ->
            "http://terminusdb.com/schema/woql#"

        Xsd ->
            "http://www.w3.org/2001/XMLSchema#"


{-| Look up a prefix in the provided context and return the associated url,
defaulting to the entire url.
-}
uriFromContext : Context -> String -> String
uriFromContext c prefix =
    Dict.get prefix c
        |> Maybe.withDefault prefix


{-| Get a list of alternatives to use as prefix for a specified schema url,
falling back to the entire url, from the provided context.
-}
fromContext : Context -> String -> List String
fromContext c u =
    Dict.foldl
        (\k v p ->
            if v == u then
                k :: p

            else
                p
        )
        [ u ]
        c
