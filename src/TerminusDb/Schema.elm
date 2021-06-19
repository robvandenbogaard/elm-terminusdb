module TerminusDb.Schema exposing
    ( Value(..), TranslatedText
    , translation, translatedText, addTranslation
    , field, literal, value
    , prefixed, requireType, andMap
    )

{-| This library covers working with Terminus (XSD, OWL) data schemata, such as
decoding json responses with schema prefix contexts into validated values.


# Values

@docs Value, TranslatedText


# Translated text helpers

@docs translation, translatedText, addTranslation


# Decoders

@docs field, literal, value


# Decoding helpers

@docs prefixed, requireType, andMap

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import TerminusDb.Schema.Prefix as Prefix exposing (Prefix)


{-| Represents a literal or node reference value.
-}
type Value
    = StringLiteral String
    | IntLiteral Int
    | TextLiteral TranslatedText
    | Reference Prefix String


{-| A TranslatedText holds a dict with translations per language (keys should be
language codes, values the translated text.)
-}
type alias TranslatedText =
    Dict String String


{-| When a document has been partially decoded by a generic document decoder,
andMap is used to decode the remaining fields needed to satisfy the specific
document type, and apply them to the (partially applied) constructor, as seen in
Schema.System.User.decode.
-}
andMap : Decoder a -> Decoder (a -> value) -> Decoder value
andMap =
    Decode.map2 (|>)


{-| Decoder that first decodes the schema prefix context from a response and
subsequently applies the provided context dependent decoder.
-}
prefixed : (Prefix.Context -> Decoder value) -> Decoder value
prefixed decoder =
    Prefix.decodeContext
        |> Decode.andThen decoder


{-| Decoder that requires a certain data type to match the type specified in the
value that is being decoded.
-}
requireType : Prefix.Context -> Prefix -> String -> Decoder a -> Decoder a
requireType context prefix type_ decoder =
    always
        decoder
        (Decode.field "@type" Decode.string
            |> Decode.andThen (match context prefix type_)
        )


{-| Helper for matching xml schema data types taking prefixes into account.
-}
match : Prefix.Context -> Prefix -> String -> String -> Decoder String
match context expectedPrefix expectedType type_ =
    let
        expectedPrefixUri =
            Prefix.uri expectedPrefix

        ( actualPrefix, actualType ) =
            prefixAndType type_

        actualPrefixUri =
            Prefix.uriFromContext context actualPrefix
    in
    if (expectedPrefixUri == actualPrefixUri) && (expectedType == actualType) then
        Decode.succeed (actualPrefix ++ ":" ++ actualType)

    else
        Decode.fail
            ("Expected "
                ++ expectedPrefixUri
                ++ ":"
                ++ expectedType
                ++ ", but got "
                ++ actualPrefixUri
                ++ ":"
                ++ actualType
            )


prefixAndType : String -> ( String, String )
prefixAndType type_ =
    case String.split ":" type_ of
        p :: t :: _ ->
            ( p, t )

        t :: [] ->
            ( "", t )

        [] ->
            ( "", "" )


{-| Decoder for fields taking the schema prefix context into account.
-}
field : Prefix.Context -> Prefix -> String -> Decoder a -> Decoder a
field context prefix name decoder =
    Prefix.fromContext context (Prefix.uri prefix)
        |> List.map
            (\p ->
                if p == "" then
                    Decode.field name decoder

                else
                    Decode.field (p ++ ":" ++ name) decoder
            )
        |> Decode.oneOf


{-| Decoder for translating a specific literal string into a boolean value.
-}
literal : String -> Decoder Bool
literal v =
    Decode.string
        |> Decode.andThen
            (\v_ ->
                if v_ == v then
                    Decode.succeed True

                else
                    Decode.succeed False
            )


{-| Decoder for translating a specific literal string value into a specified
instance of a (custom) data type.
-}
value : Prefix.Context -> Prefix -> String -> a -> Decoder a
value context prefix name instance =
    Prefix.fromContext context (Prefix.uri prefix)
        |> List.map
            (\p ->
                Decode.string
                    |> Decode.andThen
                        (\s ->
                            if s == (p ++ ":" ++ name) then
                                Decode.succeed instance

                            else
                                Decode.fail "Value does not match"
                        )
            )
        |> Decode.oneOf


{-| Decoder for building a dictionary of translations from the list of @language
@value pairs in the value that is being decoded.
-}
translatedText : Decoder TranslatedText
translatedText =
    let
        textDecoder =
            Decode.map2 Tuple.pair
                (Decode.field "@language" Decode.string)
                (Decode.field "@value" Decode.string)
    in
    Decode.map Dict.fromList <|
        Decode.oneOf
            [ Decode.andThen (\t -> Decode.succeed [ t ]) textDecoder
            , Decode.list textDecoder
            ]


{-| Create a single translation from a language identifier string and text
content.
-}
translation : String -> String -> TranslatedText
translation =
    Dict.singleton


{-| Add a translation for a specified language to a translations dictionary.
-}
addTranslation : String -> String -> TranslatedText -> TranslatedText
addTranslation =
    Dict.insert
