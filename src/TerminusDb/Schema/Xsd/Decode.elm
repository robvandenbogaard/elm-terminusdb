module TerminusDb.Schema.Xsd.Decode exposing (string, integer, nonNegativeInteger, dateTime)

{-| This module provides decoders for Xsd data types.

@docs string, integer, nonNegativeInteger, dateTime

-}

import Json.Decode as Decode exposing (Decoder)
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix


{-| Decoder for string values.
-}
string : Prefix.Context -> Decoder String
string context =
    Schema.requireType context Prefix.Xsd "string" <|
        Decode.field "@value" Decode.string


{-| Decoder for generic integer values.
-}
integer : Prefix.Context -> Decoder Int
integer context =
    Schema.requireType context Prefix.Xsd "integer" <|
        Decode.field "@value" Decode.int


{-| Decoder for non negative integers.
-}
nonNegativeInteger : Prefix.Context -> Decoder Int
nonNegativeInteger context =
    Schema.requireType context Prefix.Xsd "nonNegativeInteger" <|
        Decode.field "@value" Decode.int


{-| Decoder for dataTime values - for now decoded to string.
-}
dateTime : Prefix.Context -> Decoder String
dateTime context =
    Schema.requireType context Prefix.Xsd "dateTime" <|
        Decode.field "@value" Decode.string
