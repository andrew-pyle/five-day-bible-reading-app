module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Time exposing (millisToPosix)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { weekOfYear : Int }


init : Model
init =
    Model 1



-- UPDATE


type Msg
    = PreviousDay
    | NextDay


update : Msg -> Model -> Model
update msg model =
    case msg of
        PreviousDay ->
            if model.weekOfYear > 1 then
                { model | weekOfYear = model.weekOfYear - 1 }

            else
                model

        NextDay ->
            if model.weekOfYear < 52 then
                { model | weekOfYear = model.weekOfYear + 1 }

            else
                model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Five-Day Reading Plan Electronic Log" ]
        , button [ onClick PreviousDay ] [ text "Previous Day" ]
        , h2 [] [ text <| "Week" ++ " " ++ String.fromInt model.weekOfYear ]
        , button [ onClick NextDay ] [ text "Next Day" ]
        , ul []
            (case Dict.get model.weekOfYear fiveDayPlanData of
                Just readingReference ->
                    List.map (\dayText -> li [] [ text dayText ]) readingReference

                Nothing ->
                    [ li [] [ text "😰 No Reading found for this week!" ] ]
            )
        ]



-- HELPERS


fiveDayPlanData : Dict Int (List String)
fiveDayPlanData =
    Dict.fromList
        [ ( 1
          , [ "Genesis 1-2; Psalm 19; Mark 1"
            , "Gen 3-5; Mark 2"
            , "Gen 6-8; Psalm 104; Mark 3"
            , "Gen 9-11; Mark 4"
            , "Gen 12-15; Psalm 148; Mark 5"
            ]
          )
        , ( 2
          , [ "Genesis 16-18; Mark 6"
            , "Gen 19-20; Psalm 1; Mark 7"
            , "Gen 21-23; Psalm 107; Mark 8"
            , "Gen 24-25; Psalm 4; Mark 9"
            , "Gen 26-27; Mark 10"
            ]
          )
        , ( 3
          , [ "Genesis 28-29; Mark 11"
            , "Gen 30-31; Psalm 11; Mark 12"
            , "Gen 32-34; Psalm 145; Mark 13"
            , "Gen 35-37; Psalm 12; Mark 14;"
            , "Gen 38-40; Mark 15"
            ]
          )
        , ( 4
          , [ "Genesis 41-42; Mark 16"
            , "Gen 43-44; Psalm 24; Galatians 1"
            , "Gen 45-46; Psalm 108; Gal 2"
            , "Gen 47-48; Psalm 25; Gal 3"
            , "Gen 49-50; Gal 4"
            ]
          )
        , ( 5
          , [ "Exodus 1-3; Gal 5"
            , "Exodus 4-6; Gal 6"
            , "Exodus 7-9; Psalm 105; Ephesians 1"
            , "Exodus 10-12; Eph 2"
            , "Exodus 13-15; Psalm 114; Eph 3"
            ]
          )
        ]
