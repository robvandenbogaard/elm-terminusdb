module TerminusDb.Api.Query exposing
    ( command, Request
    , request
    )

{-| This module provides the api call `command` to perform a TerminusDB query.

It gets configured by the `Request` data type, constructed by the `request`
helper.

@docs command, Request

@docs request

-}

import Http
import Json.Decode as Decode exposing (Decoder)
import TerminusDb exposing (databasePath)
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix exposing (Prefix)
import TerminusDb.Session exposing (Session)
import TerminusDb.Woql as Woql
import Url.Builder


{-| Represents a Query request.
-}
type alias Request a msg =
    { message : Result Woql.Error a -> msg
    , decoder : Prefix.Context -> Decoder a
    , commit : Maybe Woql.CommitInfo
    , prefixes : List Prefix
    , query : Woql.Query
    }


{-| Request builder with defaults.
-}
request : (Result Woql.Error a -> msg) -> (Prefix.Context -> Decoder a) -> Woql.Query -> Request a msg
request message decoder query =
    { message = message
    , commit = Nothing
    , decoder = decoder
    , prefixes = [ Prefix.Scm, Prefix.System, Prefix.Xsd ]
    , query = query
    }


{-| Query request command builder, using the provided session for auth token,
connection parameters and schema context.
-}
command : Session -> Request a msg -> Cmd msg
command { server, database, context, token } { message, decoder, commit, prefixes, query } =
    let
        r =
            case commit of
                Nothing ->
                    Woql.QueryRequest prefixes query

                Just info ->
                    Woql.QueryCommitRequest prefixes query info
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" token ]
        , url =
            Url.Builder.crossOrigin server
                ("api" :: "woql" :: databasePath database)
                []
        , body = Http.jsonBody (Woql.request r)
        , expect = Woql.expectJson message (Decode.field "bindings" <| Schema.prefixed decoder)
        , timeout = Nothing
        , tracker = Nothing
        }
