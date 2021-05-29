module Bikes exposing (main)

-- Retrieving a list of bike journey data from a TerminusDB instance

import Browser
import Html
import Html.Events
import Http
import Json.Decode as Decode
import TerminusDb.Api.Connect as Connect
import TerminusDb.Api.Query as Query
import TerminusDb.Schema.Prefix exposing (Prefix(..))
import TerminusDb.Schema.Xsd.Decode as XsdDecode
import TerminusDb.Session exposing (Session)
import TerminusDb.Woql as Woql exposing (Query(..), Value(..))


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = NotConnected
    | Connecting
    | Connected Session
    | ConnectedGettingList Session
    | ConnectedWithList Session (List Journey)
    | Error String


type Msg
    = Clicked
    | GotConnected (Result Woql.Error Session)
    | GotList (Result Woql.Error (List Journey))


type alias Journey =
    { time : String
    , bike : String
    , start : String
    , end : String
    }


init : () -> ( Model, Cmd msg )
init flags =
    ( NotConnected, Cmd.none )


view model =
    case model of
        NotConnected ->
            { title = "Bikes - Not Connected"
            , body = [ Html.button [ Html.Events.onClick Clicked ] [ Html.text "Connect" ] ]
            }

        Connecting ->
            { title = "Bikes - Connecting"
            , body = [ Html.text "Connecting.." ]
            }

        Connected session ->
            { title = "Bikes - Connected"
            , body = [ Html.button [ Html.Events.onClick Clicked ] [ Html.text "List" ] ]
            }

        ConnectedGettingList session ->
            { title = "Bikes - Connected, getting list"
            , body = [ Html.text "Getting list.." ]
            }

        ConnectedWithList session journeys ->
            let
                viewJourney journey =
                    Html.tr []
                        [ Html.td [] [ Html.text journey.time ]
                        , Html.td [] [ Html.text journey.bike ]
                        , Html.td [] [ Html.text journey.start ]
                        , Html.td [] [ Html.text journey.end ]
                        ]
            in
            { title = "Bikes - Connected, got list"
            , body =
                [ Html.table [] <|
                    List.map viewJourney journeys
                ]
            }

        Error reason ->
            { title = "Bikes - Error"
            , body = [ Html.pre [] [ Html.text reason ] ]
            }


update msg model =
    case msg of
        Clicked ->
            case model of
                NotConnected ->
                    ( Connecting
                    , Connect.request GotConnected
                        |> Connect.toDatabase "bikes"
                        |> Connect.asUser "admin"
                        |> Connect.withPassword "root"
                        |> Connect.command
                    )

                Connected session ->
                    let
                        decoder context =
                            Decode.list <|
                                Decode.map4
                                    Journey
                                    (Decode.field "StartTime" <| XsdDecode.dateTime context)
                                    (Decode.field "Bike" <| XsdDecode.string context)
                                    (Decode.field "Start" <| XsdDecode.string context)
                                    (Decode.field "End" <| XsdDecode.string context)
                    in
                    ( ConnectedGettingList session
                    , Query.command session
                        (Query.request GotList decoder <|
                            Limit 20 <|
                                Select [ "StartTime", "Bike", "Start", "End" ]
                                    (And
                                        [ Triple (Var "JourneyRef") (Node Rdf "type") (Node Scm "Journey")
                                        , Triple (Var "JourneyRef") (Node Scm "start_time") (Var "StartTime")
                                        , Triple (Var "JourneyRef") (Node Scm "start_station") (Var "StartRef")
                                        , Triple (Var "StartRef") (Node Rdfs "label") (Var "Start")
                                        , Triple (Var "JourneyRef") (Node Scm "end_station") (Var "EndRef")
                                        , Triple (Var "EndRef") (Node Rdfs "label") (Var "End")
                                        , Triple (Var "JourneyRef") (Node Scm "journey_bicycle") (Var "BikeRef")
                                        , Triple (Var "BikeRef") (Node Rdfs "label") (Var "Bike")
                                        ]
                                    )
                        )
                    )

                _ ->
                    ( model, Cmd.none )

        GotConnected (Ok session) ->
            ( Connected session, Cmd.none )

        GotConnected (Err reason) ->
            ( Error <| errorToString reason, Cmd.none )

        GotList (Ok list) ->
            case model of
                ConnectedGettingList session ->
                    ( ConnectedWithList session list, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotList (Err reason) ->
            ( Error <| errorToString reason, Cmd.none )


errorToString : Woql.Error -> String
errorToString error =
    case error of
        Woql.BadConnection reason ->
            reason

        Woql.BadResponse status reason ->
            "HTTP status " ++ String.fromInt status ++ ":\n" ++ reason

        Woql.BadData reason ->
            reason
