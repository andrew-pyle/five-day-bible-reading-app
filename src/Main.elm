module Main exposing (..)

import Bible
import Browser
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


type alias Date =
    -- (Year, Month, Date) Month is 1-12
    ( Int, Int, Int )


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


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


dateToWeekNum : Dict ( Int, Int, Int ) Int
dateToWeekNum =
    Dict.fromList
        [ ( ( 2020, 1, 26 ), 4 )
        , ( ( 2020, 1, 27 ), 4 )
        , ( ( 2020, 1, 28 ), 4 )
        , ( ( 2020, 1, 29 ), 4 )
        , ( ( 2020, 1, 30 ), 4 )
        , ( ( 2020, 1, 31 ), 4 )
        , ( ( 2020, 2, 1 ), 4 )
        , ( ( 2020, 2, 2 ), 5 )
        , ( ( 2020, 2, 3 ), 5 )
        , ( ( 2020, 2, 4 ), 5 )
        , ( ( 2020, 2, 5 ), 5 )
        , ( ( 2020, 2, 6 ), 5 )
        , ( ( 2020, 2, 7 ), 5 )
        , ( ( 2020, 2, 8 ), 5 )
        , ( ( 2020, 2, 9 ), 6 )
        , ( ( 2020, 2, 10 ), 6 )
        , ( ( 2020, 2, 11 ), 6 )
        , ( ( 2020, 2, 12 ), 6 )
        , ( ( 2020, 2, 13 ), 6 )
        , ( ( 2020, 2, 14 ), 6 )
        , ( ( 2020, 2, 15 ), 6 )
        , ( ( 2020, 2, 16 ), 7 )
        , ( ( 2020, 2, 17 ), 7 )
        , ( ( 2020, 2, 18 ), 7 )
        , ( ( 2020, 2, 19 ), 7 )
        , ( ( 2020, 2, 20 ), 7 )
        , ( ( 2020, 2, 21 ), 7 )
        , ( ( 2020, 2, 22 ), 7 )
        , ( ( 2020, 2, 23 ), 8 )
        , ( ( 2020, 2, 24 ), 8 )
        , ( ( 2020, 2, 25 ), 8 )
        , ( ( 2020, 2, 26 ), 8 )
        , ( ( 2020, 2, 27 ), 8 )
        , ( ( 2020, 2, 28 ), 8 )
        , ( ( 2020, 2, 29 ), 8 )
        , ( ( 2020, 3, 1 ), 9 )
        , ( ( 2020, 3, 2 ), 9 )
        , ( ( 2020, 3, 3 ), 9 )
        , ( ( 2020, 3, 4 ), 9 )
        , ( ( 2020, 3, 5 ), 9 )
        , ( ( 2020, 3, 6 ), 9 )
        , ( ( 2020, 3, 7 ), 9 )
        , ( ( 2020, 3, 8 ), 10 )
        , ( ( 2020, 3, 9 ), 10 )
        , ( ( 2020, 3, 10 ), 10 )
        , ( ( 2020, 3, 11 ), 10 )
        , ( ( 2020, 3, 12 ), 10 )
        , ( ( 2020, 3, 13 ), 10 )
        , ( ( 2020, 3, 14 ), 10 )
        , ( ( 2020, 3, 15 ), 11 )
        , ( ( 2020, 3, 16 ), 11 )
        , ( ( 2020, 3, 17 ), 11 )
        , ( ( 2020, 3, 18 ), 11 )
        , ( ( 2020, 3, 19 ), 11 )
        , ( ( 2020, 3, 20 ), 11 )
        , ( ( 2020, 3, 21 ), 11 )
        , ( ( 2020, 3, 22 ), 12 )
        , ( ( 2020, 3, 23 ), 12 )
        , ( ( 2020, 3, 24 ), 12 )
        , ( ( 2020, 3, 25 ), 12 )
        , ( ( 2020, 3, 26 ), 12 )
        , ( ( 2020, 3, 27 ), 12 )
        , ( ( 2020, 3, 28 ), 12 )
        , ( ( 2020, 3, 29 ), 13 )
        , ( ( 2020, 3, 30 ), 13 )
        , ( ( 2020, 3, 31 ), 13 )
        , ( ( 2020, 4, 1 ), 13 )
        , ( ( 2020, 4, 2 ), 13 )
        , ( ( 2020, 4, 3 ), 13 )
        , ( ( 2020, 4, 4 ), 13 )
        , ( ( 2020, 4, 5 ), 14 )
        , ( ( 2020, 4, 6 ), 14 )
        , ( ( 2020, 4, 7 ), 14 )
        , ( ( 2020, 4, 8 ), 14 )
        , ( ( 2020, 4, 9 ), 14 )
        , ( ( 2020, 4, 10 ), 14 )
        , ( ( 2020, 4, 11 ), 14 )
        , ( ( 2020, 4, 12 ), 15 )
        , ( ( 2020, 4, 13 ), 15 )
        , ( ( 2020, 4, 14 ), 15 )
        , ( ( 2020, 4, 15 ), 15 )
        , ( ( 2020, 4, 16 ), 15 )
        , ( ( 2020, 4, 17 ), 15 )
        , ( ( 2020, 4, 18 ), 15 )
        , ( ( 2020, 4, 19 ), 16 )
        , ( ( 2020, 4, 20 ), 16 )
        , ( ( 2020, 4, 21 ), 16 )
        , ( ( 2020, 4, 22 ), 16 )
        , ( ( 2020, 4, 23 ), 16 )
        , ( ( 2020, 4, 24 ), 16 )
        , ( ( 2020, 4, 25 ), 16 )
        , ( ( 2020, 4, 20 ), 17 )
        , ( ( 2020, 4, 21 ), 17 )
        , ( ( 2020, 4, 22 ), 17 )
        , ( ( 2020, 4, 23 ), 17 )
        , ( ( 2020, 4, 24 ), 17 )
        , ( ( 2020, 4, 25 ), 17 )
        , ( ( 2020, 4, 26 ), 17 )
        , ( ( 2020, 4, 27 ), 17 )
        , ( ( 2020, 4, 28 ), 17 )
        , ( ( 2020, 4, 29 ), 17 )
        , ( ( 2020, 4, 30 ), 17 )
        , ( ( 2020, 5, 1 ), 17 )
        , ( ( 2020, 5, 2 ), 17 )
        , ( ( 2020, 5, 3 ), 18 )
        , ( ( 2020, 5, 4 ), 18 )
        , ( ( 2020, 5, 5 ), 18 )
        , ( ( 2020, 5, 6 ), 18 )
        , ( ( 2020, 5, 7 ), 18 )
        , ( ( 2020, 5, 8 ), 18 )
        , ( ( 2020, 5, 9 ), 18 )
        , ( ( 2020, 5, 10 ), 19 )
        , ( ( 2020, 5, 11 ), 19 )
        , ( ( 2020, 5, 12 ), 19 )
        , ( ( 2020, 5, 13 ), 19 )
        , ( ( 2020, 5, 14 ), 19 )
        , ( ( 2020, 5, 15 ), 19 )
        , ( ( 2020, 5, 16 ), 19 )
        , ( ( 2020, 5, 17 ), 20 )
        , ( ( 2020, 5, 18 ), 20 )
        , ( ( 2020, 5, 19 ), 20 )
        , ( ( 2020, 5, 20 ), 20 )
        , ( ( 2020, 5, 21 ), 20 )
        , ( ( 2020, 5, 22 ), 20 )
        , ( ( 2020, 5, 23 ), 20 )
        , ( ( 2020, 5, 24 ), 21 )
        , ( ( 2020, 5, 25 ), 21 )
        , ( ( 2020, 5, 26 ), 21 )
        , ( ( 2020, 5, 27 ), 21 )
        , ( ( 2020, 5, 28 ), 21 )
        , ( ( 2020, 5, 29 ), 21 )
        , ( ( 2020, 5, 30 ), 21 )
        , ( ( 2020, 5, 31 ), 22 )
        , ( ( 2020, 6, 1 ), 22 )
        , ( ( 2020, 6, 2 ), 22 )
        , ( ( 2020, 6, 3 ), 22 )
        , ( ( 2020, 6, 4 ), 22 )
        , ( ( 2020, 6, 5 ), 22 )
        , ( ( 2020, 6, 6 ), 22 )
        , ( ( 2020, 6, 7 ), 23 )
        , ( ( 2020, 6, 8 ), 23 )
        , ( ( 2020, 6, 9 ), 23 )
        , ( ( 2020, 6, 10 ), 23 )
        , ( ( 2020, 6, 11 ), 23 )
        , ( ( 2020, 6, 12 ), 23 )
        , ( ( 2020, 6, 13 ), 23 )
        , ( ( 2020, 6, 14 ), 24 )
        , ( ( 2020, 6, 15 ), 24 )
        , ( ( 2020, 6, 16 ), 24 )
        , ( ( 2020, 6, 17 ), 24 )
        , ( ( 2020, 6, 18 ), 24 )
        , ( ( 2020, 6, 19 ), 24 )
        , ( ( 2020, 6, 20 ), 24 )
        , ( ( 2020, 6, 21 ), 25 )
        , ( ( 2020, 6, 22 ), 25 )
        , ( ( 2020, 6, 23 ), 25 )
        , ( ( 2020, 6, 24 ), 25 )
        , ( ( 2020, 6, 25 ), 25 )
        , ( ( 2020, 6, 26 ), 25 )
        , ( ( 2020, 6, 27 ), 25 )
        , ( ( 2020, 6, 28 ), 26 )
        , ( ( 2020, 6, 29 ), 26 )
        , ( ( 2020, 6, 30 ), 26 )
        , ( ( 2020, 7, 1 ), 26 )
        , ( ( 2020, 7, 2 ), 26 )
        , ( ( 2020, 7, 3 ), 26 )
        , ( ( 2020, 7, 4 ), 26 )
        , ( ( 2020, 7, 5 ), 27 )
        , ( ( 2020, 7, 6 ), 27 )
        , ( ( 2020, 7, 7 ), 27 )
        , ( ( 2020, 7, 8 ), 27 )
        , ( ( 2020, 7, 9 ), 27 )
        , ( ( 2020, 7, 10 ), 27 )
        , ( ( 2020, 7, 11 ), 27 )
        , ( ( 2020, 7, 12 ), 28 )
        , ( ( 2020, 7, 13 ), 28 )
        , ( ( 2020, 7, 14 ), 28 )
        , ( ( 2020, 7, 15 ), 28 )
        , ( ( 2020, 7, 16 ), 28 )
        , ( ( 2020, 7, 17 ), 28 )
        , ( ( 2020, 7, 18 ), 28 )
        , ( ( 2020, 7, 19 ), 29 )
        , ( ( 2020, 7, 20 ), 29 )
        , ( ( 2020, 7, 21 ), 29 )
        , ( ( 2020, 7, 22 ), 29 )
        , ( ( 2020, 7, 23 ), 29 )
        , ( ( 2020, 7, 24 ), 29 )
        , ( ( 2020, 7, 25 ), 29 )
        , ( ( 2020, 7, 26 ), 30 )
        , ( ( 2020, 7, 27 ), 30 )
        , ( ( 2020, 7, 28 ), 30 )
        , ( ( 2020, 7, 29 ), 30 )
        , ( ( 2020, 7, 30 ), 30 )
        , ( ( 2020, 7, 31 ), 30 )
        , ( ( 2020, 8, 1 ), 30 )
        , ( ( 2020, 8, 2 ), 31 )
        , ( ( 2020, 8, 3 ), 31 )
        , ( ( 2020, 8, 4 ), 31 )
        , ( ( 2020, 8, 5 ), 31 )
        , ( ( 2020, 8, 6 ), 31 )
        , ( ( 2020, 8, 7 ), 31 )
        , ( ( 2020, 8, 8 ), 31 )
        , ( ( 2020, 8, 9 ), 32 )
        , ( ( 2020, 8, 10 ), 32 )
        , ( ( 2020, 8, 11 ), 32 )
        , ( ( 2020, 8, 12 ), 32 )
        , ( ( 2020, 8, 13 ), 32 )
        , ( ( 2020, 8, 14 ), 32 )
        , ( ( 2020, 8, 15 ), 32 )
        , ( ( 2020, 8, 16 ), 33 )
        , ( ( 2020, 8, 17 ), 33 )
        , ( ( 2020, 8, 18 ), 33 )
        , ( ( 2020, 8, 19 ), 33 )
        , ( ( 2020, 8, 20 ), 33 )
        , ( ( 2020, 8, 21 ), 33 )
        , ( ( 2020, 8, 22 ), 33 )
        , ( ( 2020, 8, 23 ), 34 )
        , ( ( 2020, 8, 24 ), 34 )
        , ( ( 2020, 8, 25 ), 34 )
        , ( ( 2020, 8, 26 ), 34 )
        , ( ( 2020, 8, 27 ), 34 )
        , ( ( 2020, 8, 28 ), 34 )
        , ( ( 2020, 8, 29 ), 34 )
        , ( ( 2020, 8, 30 ), 35 )
        , ( ( 2020, 8, 31 ), 35 )
        , ( ( 2020, 9, 1 ), 35 )
        , ( ( 2020, 9, 2 ), 35 )
        , ( ( 2020, 9, 3 ), 35 )
        , ( ( 2020, 9, 4 ), 35 )
        , ( ( 2020, 9, 5 ), 35 )
        , ( ( 2020, 9, 6 ), 36 )
        , ( ( 2020, 9, 7 ), 36 )
        , ( ( 2020, 9, 8 ), 36 )
        , ( ( 2020, 9, 9 ), 36 )
        , ( ( 2020, 9, 10 ), 36 )
        , ( ( 2020, 9, 11 ), 36 )
        , ( ( 2020, 9, 12 ), 36 )
        , ( ( 2020, 9, 13 ), 37 )
        , ( ( 2020, 9, 14 ), 37 )
        , ( ( 2020, 9, 15 ), 37 )
        , ( ( 2020, 9, 16 ), 37 )
        , ( ( 2020, 9, 17 ), 37 )
        , ( ( 2020, 9, 18 ), 37 )
        , ( ( 2020, 9, 19 ), 37 )
        , ( ( 2020, 9, 20 ), 38 )
        , ( ( 2020, 9, 21 ), 38 )
        , ( ( 2020, 9, 22 ), 38 )
        , ( ( 2020, 9, 23 ), 38 )
        , ( ( 2020, 9, 24 ), 38 )
        , ( ( 2020, 9, 25 ), 38 )
        , ( ( 2020, 9, 26 ), 38 )
        , ( ( 2020, 9, 27 ), 39 )
        , ( ( 2020, 9, 28 ), 39 )
        , ( ( 2020, 9, 29 ), 39 )
        , ( ( 2020, 9, 30 ), 39 )
        , ( ( 2020, 10, 1 ), 39 )
        , ( ( 2020, 10, 2 ), 39 )
        , ( ( 2020, 10, 3 ), 39 )
        , ( ( 2020, 10, 4 ), 40 )
        , ( ( 2020, 10, 5 ), 40 )
        , ( ( 2020, 10, 6 ), 40 )
        , ( ( 2020, 10, 7 ), 40 )
        , ( ( 2020, 10, 8 ), 40 )
        , ( ( 2020, 10, 9 ), 40 )
        , ( ( 2020, 10, 10 ), 40 )
        , ( ( 2020, 10, 11 ), 41 )
        , ( ( 2020, 10, 12 ), 41 )
        , ( ( 2020, 10, 13 ), 41 )
        , ( ( 2020, 10, 14 ), 41 )
        , ( ( 2020, 10, 15 ), 41 )
        , ( ( 2020, 10, 16 ), 41 )
        , ( ( 2020, 10, 17 ), 41 )
        , ( ( 2020, 10, 18 ), 42 )
        , ( ( 2020, 10, 19 ), 42 )
        , ( ( 2020, 10, 20 ), 42 )
        , ( ( 2020, 10, 21 ), 42 )
        , ( ( 2020, 10, 22 ), 42 )
        , ( ( 2020, 10, 23 ), 42 )
        , ( ( 2020, 10, 24 ), 42 )
        , ( ( 2020, 10, 25 ), 43 )
        , ( ( 2020, 10, 26 ), 43 )
        , ( ( 2020, 10, 27 ), 43 )
        , ( ( 2020, 10, 28 ), 43 )
        , ( ( 2020, 10, 29 ), 43 )
        , ( ( 2020, 10, 30 ), 43 )
        , ( ( 2020, 10, 31 ), 43 )
        , ( ( 2020, 11, 1 ), 44 )
        , ( ( 2020, 11, 2 ), 44 )
        , ( ( 2020, 11, 3 ), 44 )
        , ( ( 2020, 11, 4 ), 44 )
        , ( ( 2020, 11, 5 ), 44 )
        , ( ( 2020, 11, 6 ), 44 )
        , ( ( 2020, 11, 7 ), 44 )
        , ( ( 2020, 11, 8 ), 45 )
        , ( ( 2020, 11, 9 ), 45 )
        , ( ( 2020, 11, 10 ), 45 )
        , ( ( 2020, 11, 11 ), 45 )
        , ( ( 2020, 11, 12 ), 45 )
        , ( ( 2020, 11, 13 ), 45 )
        , ( ( 2020, 11, 14 ), 45 )
        , ( ( 2020, 11, 15 ), 46 )
        , ( ( 2020, 11, 16 ), 46 )
        , ( ( 2020, 11, 17 ), 46 )
        , ( ( 2020, 11, 18 ), 46 )
        , ( ( 2020, 11, 19 ), 46 )
        , ( ( 2020, 11, 20 ), 46 )
        , ( ( 2020, 11, 21 ), 46 )
        , ( ( 2020, 11, 22 ), 47 )
        , ( ( 2020, 11, 23 ), 47 )
        , ( ( 2020, 11, 24 ), 47 )
        , ( ( 2020, 11, 25 ), 47 )
        , ( ( 2020, 11, 26 ), 47 )
        , ( ( 2020, 11, 27 ), 47 )
        , ( ( 2020, 11, 28 ), 47 )
        , ( ( 2020, 11, 29 ), 48 )
        , ( ( 2020, 11, 30 ), 48 )
        , ( ( 2020, 12, 1 ), 48 )
        , ( ( 2020, 12, 2 ), 48 )
        , ( ( 2020, 12, 3 ), 48 )
        , ( ( 2020, 12, 4 ), 48 )
        , ( ( 2020, 12, 5 ), 48 )
        , ( ( 2020, 12, 6 ), 49 )
        , ( ( 2020, 12, 7 ), 49 )
        , ( ( 2020, 12, 8 ), 49 )
        , ( ( 2020, 12, 9 ), 49 )
        , ( ( 2020, 12, 10 ), 49 )
        , ( ( 2020, 12, 11 ), 49 )
        , ( ( 2020, 12, 12 ), 49 )
        , ( ( 2020, 12, 13 ), 50 )
        , ( ( 2020, 12, 14 ), 50 )
        , ( ( 2020, 12, 15 ), 50 )
        , ( ( 2020, 12, 16 ), 50 )
        , ( ( 2020, 12, 17 ), 50 )
        , ( ( 2020, 12, 18 ), 50 )
        , ( ( 2020, 12, 19 ), 50 )
        , ( ( 2020, 12, 20 ), 51 )
        , ( ( 2020, 12, 21 ), 51 )
        , ( ( 2020, 12, 22 ), 51 )
        , ( ( 2020, 12, 23 ), 51 )
        , ( ( 2020, 12, 24 ), 51 )
        , ( ( 2020, 12, 25 ), 51 )
        , ( ( 2020, 12, 26 ), 51 )
        , ( ( 2020, 12, 27 ), 52 )
        , ( ( 2020, 12, 28 ), 52 )
        , ( ( 2020, 12, 29 ), 52 )
        , ( ( 2020, 12, 30 ), 52 )
        , ( ( 2020, 12, 31 ), 52 )
        ]
