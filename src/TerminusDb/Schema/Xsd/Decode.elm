module TerminusDb.Schema.Xsd.Decode exposing
    ( dateTime
    , integer
    , nonNegativeInteger
    , string
    )

import Json.Decode as Decode exposing (Decoder)
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix


string : Prefix.Context -> Decoder String
string context =
    Schema.requireType context Prefix.Xsd "string" <|
        Decode.field "@value" Decode.string


integer : Prefix.Context -> Decoder Int
integer context =
    Schema.requireType context Prefix.Xsd "integer" <|
        Decode.field "@value" Decode.int


nonNegativeInteger : Prefix.Context -> Decoder Int
nonNegativeInteger context =
    Schema.requireType context Prefix.Xsd "nonNegativeInteger" <|
        Decode.field "@value" Decode.int


dateTime : Prefix.Context -> Decoder String
dateTime context =
    Schema.requireType context Prefix.Xsd "dateTime" <|
        Decode.field "@value" Decode.string
