module TerminusDb.Api.CreateDatabase exposing
    ( Request
    , command
    , forOrganisation
    , local
    , public
    , request
    , withDescription
    , withLabel
    , withSchema
    )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import TerminusDb exposing (Database(..), databasePath)
import TerminusDb.Session exposing (Session)
import TerminusDb.Woql as Woql
import Url.Builder


forOrganisation : String -> Request msg -> Request msg
forOrganisation org req =
    { req | organisation = org }


public : Bool -> Request msg -> Request msg
public isPublic req =
    { req | isPublic = isPublic }


withSchema : Bool -> Request msg -> Request msg
withSchema hasSchema req =
    { req | hasSchema = hasSchema }


local : Bool -> Request msg -> Request msg
local isLocal req =
    { req | isLocal = isLocal }


withLabel : String -> Request msg -> Request msg
withLabel label req =
    { req | label = label }


withDescription : String -> Request msg -> Request msg
withDescription description req =
    { req | comment = description }


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
        , expect = Woql.expectJson message Decode.bool
        , timeout = Nothing
        , tracker = Nothing
        }
