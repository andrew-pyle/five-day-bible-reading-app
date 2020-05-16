port module Main exposing (..)

import Bible
import Browser
import DateToWeek exposing (Date, dateToWeekNum, monthToInt)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Svg exposing (path, rect, svg)
import Svg.Attributes
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


type HttpRequest
    = NotSent
    | Loading
    | Failure Http.Error
    | Success BackendData


type AppData
    = LoadingIt
    | CantGetIt String
    | GotIt UserProgress


type alias Model =
    { today : Date
    , weekInView : Int
    , httpDataRequest : HttpRequest
    , appData : AppData
    }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue decodeUserProgress flags of
        Ok flagData ->
            ( { today = ( 2020, 1, 1 )
              , weekInView = 1
              , httpDataRequest = NotSent
              , appData = GotIt flagData
              }
            , Cmd.batch
                [ Task.perform SetViewFromDate getDateToday ]
            )

        Err err ->
            ( { today = ( 2020, 1, 1 )
              , weekInView = 1
              , httpDataRequest = NotSent
              , appData = LoadingIt
              }
            , Cmd.batch
                [ fetchFiveDayPlanData fiveDayPlanUrl
                , Task.perform SetViewFromDate getDateToday
                ]
            )



-- PORTS


port persistUserProgress : Json.Encode.Value -> Cmd msg



-- UPDATE


type Msg
    = NoOp
    | DataLoaded (Result Http.Error BackendData)
    | PersistUserProgress UserProgress
    | PreviousDay
    | NextDay
    | Today
    | SetViewFromDate Date
    | ToggleDayTextComplete Int Int



-- | ToggleViewSource


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PersistUserProgress data ->
            ( model, persistUserProgress <| encodeUserProgress data )

        DataLoaded response ->
            case response of
                Ok rawData ->
                    let
                        parsedData =
                            fiveDayPlanUserProgressParser rawData
                    in
                    ( { model | appData = GotIt parsedData, httpDataRequest = Success rawData }, Cmd.none )

                Err httpError ->
                    -- Try again on Timeout or NetworkError
                    case httpError of
                        Http.Timeout ->
                            ( { model | httpDataRequest = Failure httpError }, fetchFiveDayPlanData fiveDayPlanUrl )

                        Http.NetworkError ->
                            ( { model | httpDataRequest = Failure httpError }, fetchFiveDayPlanData fiveDayPlanUrl )

                        _ ->
                            ( { model | httpDataRequest = Failure httpError }, Cmd.none )

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

        ToggleDayTextComplete weekIndex dayIndex ->
            case model.appData of
                LoadingIt ->
                    ( model, Cmd.none )

                CantGetIt errMsg ->
                    ( model, Cmd.none )

                GotIt data ->
                    ( { model
                        | appData =
                            GotIt
                                (Dict.update weekIndex
                                    (\week ->
                                        case week of
                                            Nothing ->
                                                Nothing

                                            Just weekFound ->
                                                Just
                                                    (List.indexedMap
                                                        (\index day ->
                                                            if index == dayIndex then
                                                                { day | complete = not day.complete }

                                                            else
                                                                day
                                                        )
                                                        weekFound
                                                    )
                                    )
                                    data
                                )
                      }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "header" ]
            [ h1 [] [ text "Five-Day Reading Plan" ]
            , div [ class "week-controls" ]
                [ button [ onClick PreviousDay ] [ text "← Previous Week" ]
                , h2 [ class "week-label" ] [ text <| "Week" ++ " " ++ String.fromInt model.weekInView ]
                , button [ onClick NextDay ] [ text "Next Week →" ]
                ]
            , button [ class "navigate-today", onClick Today ] [ text "Today" ]
            ]
        , div []
            [ case model.appData of
                GotIt data ->
                    case Dict.get model.weekInView data of
                        Just readingForAWeek ->
                            ul [ class "day-text-list" ]
                                (List.indexedMap
                                    (\dayIndex eachDay ->
                                        li
                                            [ class
                                                (if eachDay.complete then
                                                    "complete"

                                                 else
                                                    "not-complete"
                                                )
                                            , onClick (ToggleDayTextComplete model.weekInView dayIndex)
                                            ]
                                            [ label [ class "day-text-content", for <| "complete-input-" ++ String.fromInt dayIndex ]
                                                [ input [ class "complete-input", name <| "complete-input-" ++ String.fromInt dayIndex, type_ "checkbox", checked eachDay.complete ]
                                                    []
                                                , svg
                                                    [ Svg.Attributes.version "1.1"
                                                    , Svg.Attributes.class "complete-indicator"
                                                    , Svg.Attributes.viewBox "0 0 522 522"
                                                    , Svg.Attributes.fill "none"
                                                    , Svg.Attributes.width "522"
                                                    , Svg.Attributes.height "522"
                                                    ]
                                                    [ rect
                                                        [ Svg.Attributes.class "box"
                                                        , Svg.Attributes.opacity "1"
                                                        , Svg.Attributes.stroke "black"
                                                        , Svg.Attributes.fill "#73BD59"
                                                        , Svg.Attributes.strokeWidth "20"
                                                        , Svg.Attributes.rx "110"
                                                        , Svg.Attributes.width "502"
                                                        , Svg.Attributes.height "502"
                                                        , Svg.Attributes.x "10"
                                                        , Svg.Attributes.y "10"
                                                        ]
                                                        []
                                                    , path
                                                        [ Svg.Attributes.class "check"
                                                        , Svg.Attributes.strokeWidth "40"
                                                        , Svg.Attributes.strokeOpacity "1"
                                                        , Svg.Attributes.stroke "#EDEDED"
                                                        , Svg.Attributes.strokeLinecap "round"
                                                        , Svg.Attributes.strokeLinejoin "round"
                                                        , Svg.Attributes.d "M98 318L227 423L441 95"
                                                        ]
                                                        []
                                                    ]
                                                , span []
                                                    [ text <|
                                                        (eachDay.dayText
                                                            |> List.sortWith Bible.comparePassage
                                                            |> List.map Bible.passageToString
                                                            |> List.intersperse " , "
                                                            |> List.foldl (++) ""
                                                        )
                                                    ]
                                                ]
                                            ]
                                    )
                                    readingForAWeek
                                )

                        Nothing ->
                            p [] [ text "😰 No Reading found for this week!" ]

                LoadingIt ->
                    p [] [ text "Loading..." ]

                CantGetIt errMsg ->
                    p [] [ text "\u{1F92A} We're having some trouble connecting..." ]
            ]
        , div []
            [ button
                [ onClick <|
                    case model.appData of
                        GotIt data ->
                            PersistUserProgress data

                        LoadingIt ->
                            NoOp

                        CantGetIt errMsg ->
                            NoOp
                ]
                [ text "💾 Save in Your Browser" ]
            ]
        ]



-- HELPERS
-- HTTP Request


fiveDayPlanUrl : String
fiveDayPlanUrl =
    "/data/five-day-reading-plan.json"


fetchFiveDayPlanData : String -> Cmd Msg
fetchFiveDayPlanData url =
    Http.get
        { url = url
        , expect = Http.expectJson DataLoaded fiveDayPlanRawDecoder
        }



-- App Data Structures


type alias DayText =
    List Bible.Passage


type alias TrackedDayText =
    { dayText : DayText
    , complete : Bool
    }


type alias UserWeekProgress =
    List TrackedDayText


type alias UserProgress =
    Dict Int UserWeekProgress



-- API Data Structures


type alias WeekText =
    List DayText


type alias WeekJson =
    { week : Int
    , text : WeekText
    }


type alias BackendData =
    List WeekJson


dayTextDecoder : Decoder DayText
dayTextDecoder =
    Json.Decode.list Bible.passageDecoder


weekTextDecoder : Decoder WeekText
weekTextDecoder =
    Json.Decode.list dayTextDecoder


trackedDayTextDecoder : Decoder TrackedDayText
trackedDayTextDecoder =
    Json.Decode.map2 TrackedDayText
        (Json.Decode.field "dayText" dayTextDecoder)
        (Json.Decode.field "complete" Json.Decode.bool)



-- API Data Structures Json Decoders


weekJsonDecoder : Decoder WeekJson
weekJsonDecoder =
    Json.Decode.map2 WeekJson
        (Json.Decode.field "week" Json.Decode.int)
        (Json.Decode.field "text" weekTextDecoder)


fiveDayPlanRawDecoder : Decoder (List WeekJson)
fiveDayPlanRawDecoder =
    Json.Decode.list <| weekJsonDecoder


fiveDayPlanUserProgressParser : BackendData -> UserProgress
fiveDayPlanUserProgressParser rawList =
    -- Transform HTTP data to App Data structure. Initializes each day's reading
    -- with complete = false
    let
        dictList : List ( Int, UserWeekProgress )
        dictList =
            List.map
                (\json ->
                    ( json.week
                    , initializeUserWeekData json.text
                    )
                )
                rawList
    in
    Dict.fromList dictList


initializeUserWeekData : WeekText -> UserWeekProgress
initializeUserWeekData weekText =
    -- Transforms a list of DayText into a TrackedDayText record with complete = false
    List.map (\dayText -> TrackedDayText dayText False) weekText



-- TASKS for Dates


getDateToday : Task.Task x Date
getDateToday =
    Task.map3 (\year month date -> ( year, monthToInt month, date ))
        (Task.map2 Time.toYear Time.here Time.now)
        (Task.map2 Time.toMonth Time.here Time.now)
        (Task.map2 Time.toDay Time.here Time.now)



-- ENCODERS for persisted App Data


encodeUserProgress : UserProgress -> Json.Encode.Value
encodeUserProgress usrProg =
    Json.Encode.dict
        String.fromInt
        encodeUserWeekProgress
        usrProg


encodeUserWeekProgress : UserWeekProgress -> Json.Encode.Value
encodeUserWeekProgress usrWkProg =
    Json.Encode.list encodeTrackedDayText usrWkProg


encodeTrackedDayText : TrackedDayText -> Json.Encode.Value
encodeTrackedDayText trackedDayText =
    Json.Encode.object
        [ ( "dayText", encodeDayText trackedDayText.dayText )
        , ( "complete", Json.Encode.bool trackedDayText.complete )
        ]


encodeDayText : DayText -> Json.Encode.Value
encodeDayText dayText =
    Json.Encode.list
        Bible.passageEncoder
        dayText



-- Decoders for Persisted App Data


decodeUserProgressHelper : Dict String UserWeekProgress -> Decoder (Dict Int UserWeekProgress)
decodeUserProgressHelper stringKeyDict =
    -- TODO 99 isn't a good default for Maybe
    let
        stringDictList =
            Dict.toList stringKeyDict

        intDictList =
            List.map
                (\( key, value ) ->
                    case String.toInt key of
                        Just int ->
                            ( int, value )

                        Nothing ->
                            ( 99, value )
                )
                stringDictList
    in
    Json.Decode.succeed <| Dict.fromList intDictList


decodeUserProgress : Decoder UserProgress
decodeUserProgress =
    Json.Decode.dict decodeUserWeekProgress
        |> Json.Decode.andThen decodeUserProgressHelper


decodeUserWeekProgress : Decoder UserWeekProgress
decodeUserWeekProgress =
    Json.Decode.list <|
        Json.Decode.map2 TrackedDayText
            (Json.Decode.field "dayText" dayTextDecoder)
            (Json.Decode.field "complete" Json.Decode.bool)
