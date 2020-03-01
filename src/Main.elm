module Main exposing (..)

import Bible
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Request
    = Failure Http.Error
    | Loading
    | Success FiveDayPlanData


type alias Model =
    { weekInView : Int
    , dataStatus : Request
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { weekInView = 1, dataStatus = Loading }
    , Http.get
        { url = "/local/data/five-day-reading-plan.json"
        , expect = Http.expectJson DataLoaded fiveDayPlanRawDecoder
        }
    )



-- UPDATE


type Msg
    = DataLoaded (Result Http.Error BackendData)
    | PreviousDay
    | NextDay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataLoaded response ->
            case response of
                Ok rawData ->
                    let
                        parsedData =
                            fiveDayPlanRawParser rawData
                    in
                    ( { model | dataStatus = Success parsedData }, Cmd.none )

                Err httpError ->
                    ( { model | dataStatus = Failure httpError }, Cmd.none )

        PreviousDay ->
            if model.weekInView > 1 then
                ( { model | weekInView = model.weekInView - 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        NextDay ->
            if model.weekInView < 52 then
                ( { model | weekInView = model.weekInView + 1 }, Cmd.none )

            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Five-Day Reading Plan Electronic Log" ]
        , button [ onClick PreviousDay ] [ text "Previous Day" ]
        , h2 [] [ text <| "Week" ++ " " ++ String.fromInt model.weekInView ]
        , button [ onClick NextDay ] [ text "Next Day" ]
        , ul []
            (case model.dataStatus of
                Success data ->
                    case Dict.get model.weekInView data of
                        Just readingForAWeek ->
                            List.map
                                (\eachDay ->
                                    li []
                                        -- [ text <| List.foldl (passageToString >> (++)) "" eachDay ]
                                        [ text <|
                                            "☐ "
                                                --"☐ ✓"
                                                ++ (eachDay
                                                        |> List.sortWith Bible.comparePassage
                                                        |> List.map Bible.passageToString
                                                        |> List.intersperse " & "
                                                        |> List.foldl (++) ""
                                                   )
                                        ]
                                )
                                readingForAWeek

                        Nothing ->
                            [ li [] [ text "😰 No Reading found for this week!" ] ]

                Failure httpError ->
                    [ li [] [ text "Yo. Err" ] ]

                Loading ->
                    [ li [] [ text "Loading..." ] ]
            )
        ]



-- HELPERS


type alias DayText =
    List Bible.Passage


dayTextDecoder : Decoder DayText
dayTextDecoder =
    Json.Decode.list Bible.passageDecoder


type alias WeekText =
    List DayText


weekTextDecoder : Decoder WeekText
weekTextDecoder =
    Json.Decode.list dayTextDecoder


type alias FiveDayPlanData =
    Dict Int WeekText


type alias WeekJson =
    { week : Int
    , text : WeekText
    }


weekJsonDecoder : Decoder WeekJson
weekJsonDecoder =
    Json.Decode.map2 WeekJson
        (Json.Decode.field "week" Json.Decode.int)
        (Json.Decode.field "text" weekTextDecoder)


fiveDayPlanRawDecoder : Decoder (List WeekJson)
fiveDayPlanRawDecoder =
    Json.Decode.list <| weekJsonDecoder


type alias BackendData =
    List WeekJson


fiveDayPlanRawParser : List WeekJson -> Dict Int WeekText
fiveDayPlanRawParser rawList =
    let
        dictList =
            List.map
                (\obj ->
                    ( obj.week
                    , obj.text
                    )
                )
                rawList
    in
    Dict.fromList dictList
