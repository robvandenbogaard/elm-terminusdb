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


{-| Helper for setting the target database name for a Connect Request.
-}
toDatabase : String -> Request msg -> Request msg
toDatabase database req =
    { req | database = Just database }


{-| Helper for setting the target database account for a Connect Request.
-}
toOrganisation : String -> Request msg -> Request msg
toOrganisation organisation req =
    { req | organisation = Just organisation }


{-| Helper for specifying a repository reference scope session default.
-}
toRepository : RepoReference -> Request msg -> Request msg
toRepository repository req =
    { req | repository = repository }


{-| Helper for setting the database server address for a Connect Request.
-}
toServer : String -> Request msg -> Request msg
toServer server req =
    { req | server = server }


{-| Helper for setting a Graph scope session default.
-}
withGraph : Graph -> Request msg -> Request msg
withGraph graph req =
    { req | graph = graph }


{-| Helper for specifying a user account for the Connect Request.
-}
asUser : String -> Request msg -> Request msg
asUser username req =
    { req | username = username }


{-| Helper for providing a password to a Connect Request configuration.
-}
withPassword : String -> Request msg -> Request msg
withPassword password req =
    { req | password = password }


{-| Represents parameters for a Connect request.
-}
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


{-| Request builder with defaults.
-}
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


{-| Connect request command builder, using the parameters from provided Request.
-}
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


{-| Helper for building authorization tokens.
-}
authorizationToken : String -> String -> String
authorizationToken username password =
    Base64.encode (username ++ ":" ++ password)
