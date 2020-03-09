module Main exposing (..)

import Bible
import Browser
import DateToWeek exposing (Date, dateToWeekNum, monthToInt)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }



-- MODEL


type Request
    = Failure Http.Error
    | Loading
    | Success FiveDayPlanData


type alias Model =
    { today : Date
    , weekInView : Int
    , dataStatus : Request
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { today = ( 2020, 1, 1 ), weekInView = 1, dataStatus = Loading }
    , Cmd.batch
        [ fetchFiveDayPlanData fiveDayPlanUrl
        , Task.perform SetViewFromDate getDateToday
        ]
    )



-- UPDATE


type Msg
    = DataLoaded (Result Http.Error BackendData)
    | PreviousDay
    | NextDay
    | Today
    | SetViewFromDate Date


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
                    -- Try again on Timeout or NetworkError
                    case httpError of
                        Http.Timeout ->
                            ( { model | dataStatus = Failure httpError }, fetchFiveDayPlanData fiveDayPlanUrl )

                        Http.NetworkError ->
                            ( { model | dataStatus = Failure httpError }, fetchFiveDayPlanData fiveDayPlanUrl )

                        _ ->
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

        Today ->
            ( model, Task.perform SetViewFromDate getDateToday )

        SetViewFromDate date ->
            let
                todayWeekNum =
                    Dict.get date dateToWeekNum
            in
            case todayWeekNum of
                Just weekNum ->
                    ( { model | today = date, weekInView = weekNum }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h1 [] [ text "Five-Day Reading Plan Electronic Log" ]
            , button [ onClick PreviousDay ] [ text "Previous Week" ]
            , h2 [] [ text <| "Week" ++ " " ++ String.fromInt model.weekInView ]
            , button [ onClick NextDay ] [ text "Next Week" ]
            , button [ onClick Today ] [ text "Today" ]
            ]
        , div []
            [ case model.dataStatus of
                Success data ->
                    case Dict.get model.weekInView data of
                        Just readingForAWeek ->
                            ul []
                                (List.map
                                    (\eachDay ->
                                        li []
                                            --"☐ ✓"
                                            [ text <|
                                                "☐ "
                                                    ++ (eachDay
                                                            |> List.sortWith Bible.comparePassage
                                                            |> List.map Bible.passageToString
                                                            |> List.intersperse " & "
                                                            |> List.foldl (++) ""
                                                       )
                                            ]
                                    )
                                    readingForAWeek
                                )

                        Nothing ->
                            p [] [ text "😰 No Reading found for this week!" ]

                Failure httpError ->
                    p [] [ text "😰 We're having some trouble connecting..." ]

                Loading ->
                    p [] [ text "Loading..." ]
            ]
        ]



-- HELPERS


fiveDayPlanUrl : String
fiveDayPlanUrl =
    "/local/data/five-day-reading-plan.json"


fetchFiveDayPlanData : String -> Cmd Msg
fetchFiveDayPlanData url =
    Http.get
        { url = url
        , expect = Http.expectJson DataLoaded fiveDayPlanRawDecoder
        }


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


getDateToday : Task.Task x Date
getDateToday =
    Task.map3 (\year month date -> ( year, monthToInt month, date ))
        (Task.map2 Time.toYear Time.here Time.now)
        (Task.map2 Time.toMonth Time.here Time.now)
        (Task.map2 Time.toDay Time.here Time.now)
