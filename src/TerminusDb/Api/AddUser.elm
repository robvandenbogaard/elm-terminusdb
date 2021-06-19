module TerminusDb.Api.AddUser exposing
    ( command, Request
    , request, withDescription
    )

{-| This module provides the api call `command` to add a user to the TerminusDB
database.

It gets configured by the `Request` data type, constructed by the `request` and
`with..` convenience helpers, for building the Request in pipeline style.

@docs command, Request

@docs request, withDescription

-}

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import TerminusDb.Schema as Schema
import TerminusDb.Session exposing (Session)
import TerminusDb.Woql as Woql
import Url.Builder


{-| Helper for adding a description to a AddUser query.
-}
withDescription : String -> Request msg -> Request msg
withDescription description req =
    { req | comment = description }


{-| Represents an AddUser request.
-}
type alias Request msg =
    { message : Result Woql.Error Bool -> msg
    , name : String
    , password : String
    , comment : String
    }


{-| Request builder with defaults.
-}
request : (Result Woql.Error Bool -> msg) -> { name : String, password : String } -> Request msg
request message { name, password } =
    { message = message
    , name = name
    , password = password
    , comment = "User " ++ name
    }


{-| AddUser query command builder, using the provided session for auth token,
connection parameters and schema context.
-}
command : Session -> Request msg -> Cmd msg
command { server, database, context, token } { message, name, password, comment } =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" token ]
        , url = Url.Builder.crossOrigin server [ "api", "user" ] []
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "user_identifier", Encode.string ("user_" ++ name) )
                    , ( "agent_name", Encode.string name )
                    , ( "comment", Encode.string comment )
                    , ( "password", Encode.string password )
                    ]
        , expect = Woql.expectJson message <| Schema.prefixed Woql.success
        , timeout = Nothing
        , tracker = Nothing
        }
