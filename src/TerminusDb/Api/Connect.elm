module TerminusDb.Api.Connect exposing
    ( command, Request
    , request, toServer, asUser, withPassword, toOrganisation, toDatabase, toRepository, withGraph
    )

{-| This module provides the api call `command` to connect with a TerminusDB
server.

It gets configured by the `Request` data type, constructed by the `request` and
`with..` convenience helpers, for building the Request in pipeline style.

@docs command, Request

@docs request, toServer, asUser, withPassword, toOrganisation, toDatabase, toRepository, withGraph

-}

import Base64
import Http
import TerminusDb exposing (Database(..), Graph(..), GraphType(..), Reference(..), RepoReference(..))
import TerminusDb.Schema as Schema
import TerminusDb.Session as Session exposing (Session)
import TerminusDb.Woql as Woql
import Url.Builder


toDatabase : String -> Request msg -> Request msg
toDatabase database req =
    { req | database = Just database }


toOrganisation : String -> Request msg -> Request msg
toOrganisation organisation req =
    { req | organisation = Just organisation }


toRepository : RepoReference -> Request msg -> Request msg
toRepository repository req =
    { req | repository = repository }


toServer : String -> Request msg -> Request msg
toServer server req =
    { req | server = server }


withGraph : Graph -> Request msg -> Request msg
withGraph graph req =
    { req | graph = graph }


asUser : String -> Request msg -> Request msg
asUser username req =
    { req | username = username }


withPassword : String -> Request msg -> Request msg
withPassword password req =
    { req | password = password }


type alias Request msg =
    { message : Result Woql.Error Session -> msg
    , server : String
    , organisation : Maybe String
    , database : Maybe String
    , repository : RepoReference
    , graph : Graph
    , username : String
    , password : String
    }


request : (Result Woql.Error Session -> msg) -> Request msg
request message =
    { message = message
    , server = "https://127.0.0.1:6363"
    , organisation = Nothing
    , database = Nothing
    , repository = Local Main
    , graph = MainGraph Instance
    , username = "admin"
    , password = "root"
    }


command : Request msg -> Cmd msg
command { message, server, organisation, database, repository, graph, username, password } =
    let
        token =
            "Basic " ++ authorizationToken username password

        root =
            if ( organisation, database ) == ( Nothing, Nothing ) then
                Terminus

            else
                Database
                    (Maybe.withDefault "admin" organisation)
                    (Maybe.withDefault "admin" database)
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = Url.Builder.crossOrigin server [ "api/" ] []
        , body = Http.emptyBody
        , expect = Woql.expectJson message (Schema.prefixed <| Session.decoder token server root repository graph)
        , timeout = Nothing
        , tracker = Nothing
        }


authorizationToken : String -> String -> String
authorizationToken username password =
    Base64.encode (username ++ ":" ++ password)
