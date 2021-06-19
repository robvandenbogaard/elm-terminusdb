module TerminusDb.Schema.System.Document exposing
    ( Document
    , decoder, decoderFor
    )

{-| This module provides the System Document type and decoders.

@docs Document

@docs decoder, decoderFor

-}

import Json.Decode as Decode exposing (Decoder)
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix exposing (Prefix)
import Tuple


{-| Represents the Document base type.
-}
type alias Document =
    { id : String
    , comment : Schema.TranslatedText
    }


{-| Document decoder, decodes id and comment, as these are mandatory for any
document.
-}
decoder : Prefix.Context -> Decoder Document
decoder context =
    Decode.map2 Document
        (Decode.field "@id" Decode.string)
        (Schema.field context Prefix.Rdfs "comment" Schema.translatedText)


{-| Decoder for documents based upon the base Document type.
-}
decoderFor : Prefix.Context -> Prefix -> String -> (String -> Schema.TranslatedText -> value) -> Decoder value
decoderFor context prefix typeName documentType =
    Schema.requireType context prefix typeName <|
        Decode.map2 documentType
            (Decode.field "@id" Decode.string)
            (Schema.field context Prefix.Rdfs "comment" Schema.translatedText)
