module TerminusDb.Session exposing
    ( Session
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import TerminusDb exposing (Database, Graph, RepoReference)
import TerminusDb.Schema.Prefix as Prefix
import TerminusDb.Schema.System.User as User exposing (User)
import Time


type alias Session =
    { server : String
    , database : Database
    , ref : RepoReference
    , graph : Graph
    , context : Prefix.Context
    , token : String
    , log : List ( Time.Posix, LogLevel, String )
    , user : User
    }


type LogLevel
    = Info
    | Warning
    | Error


decoder : String -> String -> Database -> RepoReference -> Graph -> Prefix.Context -> Decoder Session
decoder token url db ref graph context =
    User.decoder context
        |> Decode.map (Session url db ref graph context token [])
