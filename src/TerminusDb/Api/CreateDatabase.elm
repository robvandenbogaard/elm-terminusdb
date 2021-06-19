module TerminusDb.Api.CreateDatabase exposing
    ( command, Request
    , request, withLabel, withDescription, forOrganisation, local, public, withSchema
    )

{-| This module provides the api call `command` to create a database on a
TerminusDB server.

It gets configured by the `Request` data type, constructed by the `request` and
`with..` convenience helpers, for building the Request in pipeline style.

@docs command, Request

@docs request, withLabel, withDescription, forOrganisation, local, public, withSchema

-}

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import TerminusDb exposing (Database(..), databasePath)
import TerminusDb.Schema as Schema
import TerminusDb.Session exposing (Session)
import TerminusDb.Woql as Woql
import Url.Builder


{-| Helper for providing an organisation (database account) parameter to the
request.
-}
forOrganisation : String -> Request msg -> Request msg
forOrganisation org req =
    { req | organisation = org }


{-| Helper for specifying if the target database is to be public.
-}
public : Bool -> Request msg -> Request msg
public isPublic req =
    { req | isPublic = isPublic }


{-| Helper for specifying whether the target database needs a schema.
-}
withSchema : Bool -> Request msg -> Request msg
withSchema hasSchema req =
    { req | hasSchema = hasSchema }


{-| Helper for specifying whether the target database is local or remote.
-}
local : Bool -> Request msg -> Request msg
local isLocal req =
    { req | isLocal = isLocal }


{-| Helper for providing the friendly name of the target database.
-}
withLabel : String -> Request msg -> Request msg
withLabel label req =
    { req | label = label }


{-| Helper for providing a comment describing the target database.
-}
withDescription : String -> Request msg -> Request msg
withDescription description req =
    { req | comment = description }


{-| Represents a CreateDatabase request.
-}
type alias Request msg =
    { message : Result Woql.Error Bool -> msg
    , name : String
    , organisation : String
    , label : String
    , comment : String
    , isPublic : Bool
    , hasSchema : Bool
    , isLocal : Bool
    }


{-| Request builder with defaults.
-}
request : (Result Woql.Error Bool -> msg) -> String -> Request msg
request message name =
    { message = message
    , name = name
    , organisation = "admin"
    , label = name
    , comment = name ++ " database"
    , isPublic = False
    , hasSchema = True
    , isLocal = True
    }


{-| CreateDatabase query command builder, using the provided session for auth
token, connection parameters and schema context.
-}
command : Session -> Request msg -> Cmd msg
command { server, database, context, token } { message, name, organisation, label, comment, isPublic, hasSchema, isLocal } =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" token ]
        , url =
            Url.Builder.crossOrigin server
                ("api" :: "db" :: databasePath (Database organisation name))
                []
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "id", Encode.string name )
                    , ( "schema", Encode.bool hasSchema )
                    , ( "label", Encode.string label )
                    , ( "comment", Encode.string comment )
                    , ( "public", Encode.bool isPublic )
                    , ( "sharing"
                      , case isLocal of
                            True ->
                                Encode.string "local"

                            False ->
                                Encode.string "remote"
                      )
                    ]
        , expect = Woql.expectJson message <| Schema.prefixed Woql.success
        , timeout = Nothing
        , tracker = Nothing
        }
