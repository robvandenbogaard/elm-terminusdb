module TerminusDb.Schema.Xsd.Encode exposing (string, integer, nonNegativeInteger, translatedText)

{-| This module provides encoders for Xsd data types.

@docs string, integer, nonNegativeInteger, translatedText

-}

import Dict
import Json.Encode as Encode
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix


{-| Encoder for string values.
-}
string : String -> Encode.Value
string text =
    Encode.object
        [ ( "@value", Encode.string text )
        , ( "@type", Encode.string "xsd:string" )
        ]


{-| Encoder for a single translation.
-}
translation : ( String, String ) -> Encode.Value
translation ( lang, text ) =
    Encode.object
        [ ( "@value", Encode.string text )
        , ( "@type", Encode.string "xsd:string" )
        , ( "@language", Encode.string lang )
        ]


{-| Encoder for translated text, a list of translations.
-}
translatedText : Schema.TranslatedText -> Encode.Value
translatedText =
    Encode.list translation << Dict.toList


{-| Encoder for generic integer values.
-}
integer : Int -> Encode.Value
integer value =
    Encode.object
        [ ( "@type", Encode.string "xsd:integer" )
        , ( "@value", Encode.int value )
        ]


{-| Encoder for non negative integer values.
-}
nonNegativeInteger : Int -> Encode.Value
nonNegativeInteger value =
    Encode.object
        [ ( "@type", Encode.string "xsd:nonNegativeInteger" )
        , ( "@value", Encode.int value )
        ]
