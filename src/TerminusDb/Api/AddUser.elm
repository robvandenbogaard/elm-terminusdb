module TerminusDb.Api.AddUser exposing
    ( Request
    , command
    , request
    , withDescription
    )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import TerminusDb.Session exposing (Session)
import TerminusDb.Woql as Woql
import Url.Builder


withDescription : String -> Request msg -> Request msg
withDescription description req =
    { req | comment = description }


type alias Request msg =
    { message : Result Woql.Error Bool -> msg
    , name : String
    , password : String
    , comment : String
    }


request : (Result Woql.Error Bool -> msg) -> { name : String, password : String } -> Request msg
request message { name, password } =
    { message = message
    , name = name
    , password = password
    , comment = "User " ++ name
    }


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
        , expect = Woql.expectJson message Decode.bool
        , timeout = Nothing
        , tracker = Nothing
        }
