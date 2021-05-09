module TerminusDb.Schema.Xsd.Encode exposing
    ( integer
    , nonNegativeInteger
    , string
    )

import Json.Encode as Encode
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix


string : String -> Encode.Value
string value =
    Encode.object
        [ ( "@value", Encode.string value )
        , ( "@type", Encode.string "xsd:string" )
        ]


integer : Int -> Encode.Value
integer value =
    Encode.object
        [ ( "@type", Encode.string "xsd:integer" )
        , ( "@value", Encode.int value )
        ]


nonNegativeInteger : Int -> Encode.Value
nonNegativeInteger value =
    Encode.object
        [ ( "@type", Encode.string "xsd:nonNegativeInteger" )
        , ( "@value", Encode.int value )
        ]
