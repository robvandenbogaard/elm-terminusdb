module TerminusDb.Schema.System.User exposing
    ( User
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix
import TerminusDb.Schema.System.Document as Document exposing (Document)
import TerminusDb.Schema.Xsd.Decode as Xsd


type alias User =
    { id : String
    , comment : Schema.TranslatedText
    , name : String
    }


decoder : Prefix.Context -> Decoder User
decoder context =
    Document.decoderFor context Prefix.System "User" User
        |> Schema.andMap (Schema.field context Prefix.System "agent_name" (Xsd.string context))
