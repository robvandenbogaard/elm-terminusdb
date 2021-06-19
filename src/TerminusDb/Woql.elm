module TerminusDb.Woql exposing
    ( Request(..), request, CommitInfo, commitInfo, Response, response, success, Error(..), Bindings
    , Query(..), Subject, Predicate, Object, Graph, Value(..), Variables
    , expectJson
    )

{-| Construct WOQL requests and read out responses using this module.

@docs Request, request, CommitInfo, commitInfo, Response, response, success, Error, Bindings

@docs Query, Subject, Predicate, Object, Graph, Value, Variables

@docs expectJson

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import TerminusDb.Schema as Schema
import TerminusDb.Schema.Prefix as Prefix exposing (Prefix)
import TerminusDb.Schema.Xsd.Decode as XsdDecode
import TerminusDb.Schema.Xsd.Encode as XsdEncode


{-| Represents meta information sent with a commit.
-}
type alias CommitInfo =
    { author : String
    , message : String
    }


{-| CommitInfo encoder.
-}
commitInfo : CommitInfo -> Encode.Value
commitInfo { author, message } =
    Encode.object [ ( "author", Encode.string author ), ( "message", Encode.string message ) ]


{-| Represents a WOQL response record.
-}
type alias Response =
    { success : Bool
    , variables : List String
    , bindings : Bindings
    , inserts : Int
    , deletes : Int
    , retries : Int
    }


{-| Represents the types of errors that can occur in WOQL requests.
-}
type Error
    = BadConnection String
    | BadResponse Int String
    | BadData String


{-| Represents bindings of names (dict keys) to query variables (dict values).
-}
type alias Bindings =
    List (Dict String String)


{-| Decoder for determining whether a response was successful.
-}
success : Prefix.Context -> Decoder Bool
success context =
    Schema.field context
        Prefix.Api
        "status"
        (Decode.oneOf
            [ Schema.value context Prefix.Api "success" True
            , Schema.value context Prefix.Api "failure" False
            ]
        )


{-| WOQL Response decoder.
-}
response : Prefix.Context -> Decoder Response
response context =
    Schema.requireType context Prefix.Api "WoqlResponse" <|
        Decode.map6 Response
            (success context)
            (Decode.oneOf
                [ Schema.field context Prefix.Api "variable_names" (Decode.list Decode.string)
                , Decode.succeed []
                ]
            )
            (Schema.field context Prefix.Unprefixed "bindings" (decodeBindings context))
            (Decode.field "inserts" Decode.int)
            (Decode.field "deletes" Decode.int)
            (Decode.field "transaction_retry_count" Decode.int)


{-| Decoder for WOQL response bindings.
-}
decodeBindings : Prefix.Context -> Decoder Bindings
decodeBindings context =
    Decode.list
        (Decode.dict <|
            Decode.oneOf
                [ Decode.string
                , XsdDecode.string context
                ]
        )


{-| Represents a WOQL request with or without provided commit metadata.
-}
type Request
    = QueryRequest (List Prefix) Query
    | QueryCommitRequest (List Prefix) Query CommitInfo


{-| Encodes a WOQL request with provided query.
-}
request : Request -> Encode.Value
request r =
    case r of
        QueryRequest p q ->
            Encode.object
                [ ( "query", encode p q )
                ]

        QueryCommitRequest p q c ->
            Encode.object
                [ ( "commit_info", commitInfo c )
                , ( "query", encode p q )
                ]


{-| Represents the WOQL query language.
-}
type Query
    = Select Variables Query
    | And (List Query)
    | Or (List Query)
    | Triple Subject Predicate Object
    | Optional Query
    | Limit Int Query
    | AddTriple Subject Predicate Object
    | AddQuad Subject Predicate Object Graph
    | IdGen Base Key String


{-| Represents the Subject within a triple.
-}
type alias Subject =
    Value


{-| Represents the Predicate within a triple.
-}
type alias Predicate =
    Value


{-| Represents the Object within a triple.
-}
type alias Object =
    Value


{-| Represents the Graph within a quad (triple + graph).
-}
type alias Graph =
    Value


{-| Represents the base for generating ids.
-}
type alias Base =
    Value


{-| A list of key segment values for generating ids.
-}
type alias Key =
    List Value


{-| Represents a WOQL query value, a variable, node reference, literal, or
translated data type.
-}
type Value
    = Var String
    | Node Prefix String
    | Literal String
    | Datatype Schema.TranslatedText


{-| Represents a list of WOQL variable names.
-}
type alias Variables =
    List String


encode : List Prefix -> Query -> Encode.Value
encode prefixes query =
    let
        -- this handling of contexts is a blunt workaround, but works for now
        context =
            Prefix.context <|
                [ Prefix.Woql, Prefix.Xsd ]
                    ++ prefixes
    in
    encodeSubQuery (Just context) query


encodeSubQuery : Maybe Prefix.Context -> Query -> Encode.Value
encodeSubQuery context query =
    Encode.object <|
        (case query of
            Select vars subQuery ->
                [ ( "@type", Encode.string "woql:Select" )
                , ( "woql:variable_list", encodeVariableList vars )
                , ( "woql:query", encodeSubQuery Nothing subQuery )
                ]

            And queries ->
                [ ( "@type", Encode.string "woql:And" )
                , ( "woql:query_list", encodeQueryList queries )
                ]

            Or queries ->
                [ ( "@type", Encode.string "woql:Or" )
                , ( "woql:query_list", encodeQueryList queries )
                ]

            Triple subject predicate object ->
                [ ( "@type", Encode.string "woql:Triple" )
                , ( "woql:subject", encodeValue subject )
                , ( "woql:predicate", encodeValue predicate )
                , ( "woql:object", encodeValue object )
                ]

            Optional subquery ->
                [ ( "@type", Encode.string "woql:Optional" )
                , ( "woql:query", encodeSubQuery Nothing subquery )
                ]

            Limit rows subQuery ->
                [ ( "@type", Encode.string "woql:Limit" )
                , ( "woql:limit", XsdEncode.nonNegativeInteger rows )
                , ( "woql:query", encodeSubQuery Nothing subQuery )
                ]

            AddTriple subject predicate object ->
                [ ( "@type", Encode.string "woql:AddTriple" )
                , ( "woql:subject", encodeValue subject )
                , ( "woql:predicate", encodeValue predicate )
                , ( "woql:object", encodeValue object )
                ]

            AddQuad subject predicate object graph ->
                [ ( "@type", Encode.string "woql:AddQuad" )
                , ( "woql:subject", encodeValue subject )
                , ( "woql:predicate", encodeValue predicate )
                , ( "woql:object", encodeValue object )
                , ( "woql:graph", encodeValue graph )
                ]

            IdGen base key uri ->
                [ ( "@type", Encode.string "woql:IDGenerator" )
                , ( "woql:base", encodeValue base )
                , ( "woql:key_list", encodeKeyList key )
                , ( "woql:uri", Encode.string uri )
                ]
        )
            ++ (case context of
                    Nothing ->
                        []

                    Just c ->
                        [ ( "@context", Prefix.encodeContext c ) ]
               )


encodeKeyList : List Value -> Encode.Value
encodeKeyList key =
    Encode.list
        (encodeIndexedListItem "woql:ValueListElement" "woql:value_list_element" encodeValue)
        (List.indexedMap Tuple.pair key)


encodeVariableList : List String -> Encode.Value
encodeVariableList variables =
    Encode.list
        (encodeIndexedListItem "woql:VariableListElement" "woql:variable_name" XsdEncode.string)
        (List.indexedMap Tuple.pair variables)


encodeIndexedListItem : String -> String -> (a -> Encode.Value) -> ( Int, a ) -> Encode.Value
encodeIndexedListItem typeName valueName encoder ( index, value ) =
    Encode.object
        [ ( "@type", Encode.string typeName )
        , ( valueName, encoder value )
        , ( "woql:index", XsdEncode.nonNegativeInteger index )
        ]


encodeQueryList : List Query -> Encode.Value
encodeQueryList queries =
    Encode.list
        (encodeIndexedListItem "woql:QueryListElement" "woql:query" (encodeSubQuery Nothing))
        (List.indexedMap Tuple.pair queries)


encodeValue : Value -> Encode.Value
encodeValue value =
    case value of
        Node prefix node ->
            Encode.object
                [ ( "@type", Encode.string "woql:Node" )
                , ( "woql:node", Encode.string <| Prefix.string prefix ++ ":" ++ node )
                ]

        Var name ->
            Encode.object
                [ ( "@type", Encode.string "woql:Variable" )
                , ( "woql:variable_name", XsdEncode.string name )
                ]

        Literal s ->
            XsdEncode.string s

        Datatype t ->
            Encode.object
                [ ( "@type", Encode.string "woql:Datatype" )
                , ( "woql:datatype", XsdEncode.translatedText t )
                ]


{-| Helper for building WOQL response expectations and adding error categories
for easy handling in applications.
-}
expectJson : (Result Error a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \resp ->
            case resp of
                Http.BadUrl_ url ->
                    Err <| BadConnection <| httpErrorString <| Http.BadUrl url

                Http.Timeout_ ->
                    Err <| BadConnection <| httpErrorString Http.Timeout

                Http.NetworkError_ ->
                    Err <| BadConnection <| httpErrorString Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString apiErrorDecoder body of
                        Ok reason ->
                            Err (BadData reason.message)

                        Err decodeError ->
                            Err (BadResponse metadata.statusCode <| Decode.errorToString decodeError)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err decodeError ->
                            Err (BadResponse metadata.statusCode <| Decode.errorToString decodeError)


type alias ApiError =
    { message : String
    , status : String
    }


apiErrorDecoder : Decode.Decoder ApiError
apiErrorDecoder =
    Decode.map2 ApiError
        (Decode.field "api:message" Decode.string)
        (Decode.field "api:status" Decode.string)


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus 401 ->
            "Authorization required"

        Http.BadStatus code ->
            "Error response " ++ String.fromInt code

        Http.BadBody errorMessage ->
            errorMessage
