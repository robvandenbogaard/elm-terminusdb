module TerminusDb.Schema.System.User exposing
    ( User
    , decoder
    )

{-| This module provides the System User type and decoders.

@docs User

@docs decoder

-}

import Json.Decode as Decode exposing (Decoder)
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix
import TerminusDb.Schema.System.Document as Document exposing (Document)
import TerminusDb.Schema.Xsd.Decode as Xsd


{-| Represents a database user account.
-}
type alias User =
    { id : String
    , comment : Schema.TranslatedText
    , name : String
    }


{-| Decoder for database user documents.
-}
decoder : Prefix.Context -> Decoder User
decoder context =
    Document.decoderFor context Prefix.System "User" User
        |> Schema.andMap (Schema.field context Prefix.System "agent_name" (Xsd.string context))
