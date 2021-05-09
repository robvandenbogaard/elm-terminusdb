module TerminusDb.Api.Query exposing
    ( Request
    , command
    , request
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import TerminusDb exposing (databasePath)
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix exposing (Prefix)
import TerminusDb.Session exposing (Session)
import TerminusDb.Woql as Woql
import Url.Builder


type alias Request a msg =
    { message : Result Woql.Error a -> msg
    , decoder : Prefix.Context -> Decoder a
    , commit : Maybe Woql.CommitInfo
    , prefixes : List Prefix
    , query : Woql.Query
    }


request : (Result Woql.Error a -> msg) -> (Prefix.Context -> Decoder a) -> Woql.Query -> Request a msg
request message decoder query =
    { message = message
    , commit = Nothing
    , decoder = decoder
    , prefixes = [ Prefix.Scm, Prefix.System, Prefix.Xsd ]
    , query = query
    }


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
