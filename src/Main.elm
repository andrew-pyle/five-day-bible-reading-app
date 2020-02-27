module Main exposing (..)

import Bible
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
    { weekInView : Int }


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
            if model.weekInView > 1 then
                { model | weekInView = model.weekInView - 1 }

            else
                model

        NextDay ->
            if model.weekInView < 52 then
                { model | weekInView = model.weekInView + 1 }

            else
                model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Five-Day Reading Plan Electronic Log" ]
        , button [ onClick PreviousDay ] [ text "Previous Day" ]
        , h2 [] [ text <| "Week" ++ " " ++ String.fromInt model.weekInView ]
        , button [ onClick NextDay ] [ text "Next Day" ]
        , ul []
            (case Dict.get model.weekInView fiveDayPlanDataMachine of
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
        , ( 6
          , [ "Exodus 16-18; Eph 4"
            , "Exodus 19-21; Psalm 33; Eph 5"
            , "Exodus 22-24; Psalm 109; Eph 6"
            , "Exodus 25-27; Psalm 90; Philippians 1"
            , "Exodus 28-31; Phil 2"
            ]
          )
        , ( 7
          , [ "Exodus 32-34; Philippians 3"
            , "Exodus 35-37; Psalm 26; Phil 4"
            , "Exodus 38-40; Hebrews 1"
            , "Leviticus 1-3; Psalm 27; Heb 2"
            , "Lev 4-7; Heb 3"
            ]
          )
        , ( 8
          , [ "Leviticus 8-11; Ps 110; Hebrews 4"
            , "Lev 12-14; Psalm 111; Heb 5"
            , "Lev 15-18; Psalm 31; Heb 6"
            , "Lev 19-20; Heb 7"
            , "Lev 21-23; Heb 8"
            ]
          )
        , ( 9
          , [ "Leviticus 24-25; Psalm 81; Hebrews 9"
            , "Lev 26-27; Psalm 112; Heb 10"
            , "Numbers 1-2; Psalm 64; Heb 11"
            , "Num 3-5; Heb 12"
            , "Num 6-7; Heb 13"
            ]
          )
        , ( 10
          , [ "Numbers 8-11; Colossians 1"
            , "Num 12-14; Psalm 28; Col 2"
            , "Num 15-18; Psalm 113; Col 3"
            , "Num 19-21; Col 4"
            , "Num 22-25; Luke 1"
            ]
          )
        , ( 11
          , [ "Numbers 26-29; Luke 2"
            , "Num 30-33; Psalm 35; Luke 3"
            , "Num 34-36; Luke 4"
            , "Deuteronomy 1-3; Psalm 36; Luke 5"
            , "Deut 4-5; Luke 6"
            ]
          )
        , ( 12
          , [ "Deuteronomy 6-9; Luke 7"
            , "Deut 10-14; Psalm 5; Luke 8"
            , "Deut 15-18; Psalm 115; Luke 9"
            , "Deut 19-22; Psalm 6; Luke 10"
            , "Deut 23-26; Luke 11"
            ]
          )
        , ( 13
          , [ "Deut 27-31; Luke 12"
            , "Deut 32-34; Psalm 13; Luke 13"
            , "Joshua 1-4; Psalm 143; Luke 14"
            , "Joshua 5-8; Psalm 14; Luke 15"
            , "Joshua 9-13; Luke 16"
            ]
          )
        , ( 14
          , [ "Joshua 14-17; Luke 17"
            , "Joshua 18-21; Psalm 15; Luke 18"
            , "Joshua 22-24; Psalm 116; Luke 19"
            , "Judges 1-3; Psalm 16; Luke 20"
            , "Judges 4-6; Luke 21"
            ]
          )
        , ( 15
          , [ "Judges 7-8; Luke 22"
            , "Judges 9-11; Psalm 17; Luke 23"
            , "Judges 12-16; Psalm 146; Luke 24"
            , "Judges 17-18; Psalm 21; Acts 1"
            , "Judges 19-21; Acts 2"
            ]
          )
        , ( 16
          , [ "Ruth 1-2; Acts 3"
            , "Ruth 3-4; Psalm 37; Acts 4"
            , "1 Samuel 1-2; Psalm 120; Acts 5"
            , "1 Sam 3-5; Psalm 23; Acts 6"
            , "1 Sam 6-8; Acts 7"
            ]
          )
        , ( 17
          , [ "1 Samuel 9-10; Acts 8"
            , "1 Sam 11-13; Psalm 38; Acts 9"
            , "1 Sam 14; Psalm 124; Acts 10"
            , "1 Sam 15-16; 1 Chr 1; Ps 39; Acts 11"
            , "1 Sam 17; 1 Chr 2; Acts 12"
            ]
          )
        , ( 18
          , [ "1 Sam 18-19; 1 Chr 3; Ps 59; Acts 13"
            , "1 Sa 20;1 Chr 4; Ps 56, 57, 142; Acts 14"
            , "1 Sam 21-22; 1 Chr 5; Ps 52; Acts 15"
            , "1 Sam 23-24; 1 Chr 6; Ps 54; Acts 16"
            , "1 Sam 25; 1 Chr 7; Acts 17"
            ]
          )
        , ( 19
          , [ "1 Sam 26-27; 1 Chr 8; Acts 18"
            , "1 Sam 28-29; 1 Chr 9; Acts 19"
            , "1 Sam 30-31; 1 Chr 10; Acts 20"
            , "2 Sam 1-2; 1 Chr 11; Ps 96, 106; Acts 21"
            , "2 Sam 3-5; 1 Chr 12; Ps 122; Acts 22"
            ]
          )
        , ( 20
          , [ "2 Sam 6; 1 Chr 13; Psalm 60; Acts 23"
            , "1 Chron 14-16; Acts 24"
            , "2 Sam 7-8; 1 Chr 17; Ps 132; Acts 25"
            , "2 Sam 9-10; 1 Chr 18-19; Ps 89; Acts 26"
            , "2 Sa 11-12; 1 Chr 20; Ps 51, 32; Acts 27"
            ]
          )
        , ( 21
          , [ "2 Sam 13-14; Acts 28"
            , "2 Sam 15-17; Psalms 3, 63; Romans 1"
            , "2 Sam 18-20; Psalm 34; Romans 2"
            , "2 Sam 21-23; Psalm 18; Romans 3"
            , "2 Sam 24; 1 Chr 21; Romans 4"
            ]
          )
        , ( 22
          , [ "1 Chr 22-25; Psalm 78; Romans 5"
            , "1 Kings 1; 1 Chr 26-28; Romans 6"
            , "1 Kings 2; 1 Chr 29; Romans 7"
            , "1 Kings 3; 2 Chr 1; Ps 42; Romans 8"
            , "1 Kings 4; Prov 1-2; Psalm 43; Romans 9"
            ]
          )
        , ( 23
          , [ "Proverbs 3-5; Romans 10"
            , "Proverbs 6-7; Psalm 7; Romans 11"
            , "Proverbs 8-10; Psalm 144; Romans 12"
            , "Proverbs 11-13; Ps 8; Romans 13"
            , "Proverbs 14-15; Romans 14"
            ]
          )
        , ( 24
          , [ "Proverbs 16-18; Romans 15"
            , "Proverbs 19-21; Ps 40; Romans 16"
            , "Proverbs 22-23; Ps 117; 1 Thess 1"
            , "Proverbs 24-25; Ps 41; 1 Thess 2"
            , "Proverbs 26-28; 1 Thess 3"
            ]
          )
        , ( 25
          , [ "Proverbs 29-31; 1 Thess 4"
            , "Song of Sol 1-3; Ps 72; 1 Thess 5"
            , "Song of Sol 4-6; 2 Thess 1"
            , "Song of Sol 7-8; Psalm 127; 2 Thess 2"
            , "1 Kings 5; 2 Chr 2; 2 Thess 3"
            ]
          )
        , ( 26
          , [ "1 Kings 6; 2 Chr 3; 1 Timothy 1"
            , "1 Kings 7; 2 Chr 4; Psalm 44; 1 Tim 2"
            , "1 Kings 8; Psalm 30; 1 Tim 3"
            , "2 Chr 5-7; Psalm 121; 1 Tim 4"
            , "1 Kings 9; 2 Chr 8; 1 Tim 5"
            ]
          )
        , ( 27
          , [ "1 Kings 10-11; 2 Chr 9; 1 Tim 6"
            , "Ecclesiastes 1-3; Psalm 45; 2 Tim 1"
            , "Eccl 4-6; Psalm 125; 2 Tim 2"
            , "Eccl 7-9; Psalm 46; 2 Tim 3"
            , "Eccl 10-12; 2 Tim 4"
            ]
          )
        , ( 28
          , [ "1 Kings 12; 2 Chr 10-11; Titus 1"
            , "1 Kings 13-14; 2 Chr 12; Ps 47; Titus 2"
            , "1 Kings 15; 2 Chr 13-14; Titus 3"
            , "2 Chr 15-16; 1 Kings 16; Philemon"
            , "1 Kings 17-18; Psalm 119; Jude"
            ]
          )
        , ( 29
          , [ "1 Kin 19-21; 2 Chr 17; Ps 129; Matt 1"
            , "1 Kings 22; 2 Chr 18; Matt 2"
            , "2 Chr 19-20; 2 Kings 1; Psalm 20; Matt 3"
            , "2 Kings 2-3; Psalm 48; Matt 4"
            , "2 Kings 4-6; Matt 5"
            ]
          )
        , ( 30
          , [ "2 Kings 7-8; 2 Chr 21; Matt 6"
            , "2 Kings 9-10; Psalm 49; Matt 7"
            , "2 Chr 22-23; 2 Kings 11; Ps 131; Matt 8"
            , "2 Chr 24; 2 Kings 12; Psalm 50; Matt 9"
            , "Joel; Matt 10"
            ]
          )
        , ( 31
          , [ "Jonah; Matt 11"
            , "2 Kings 13-14; 2 Chr 25; Ps 53; Matt 12"
            , "Amos 1-3; Matt 13"
            , "Amos 4-6; Psalm 55; Matt 14"
            , "Amos 7-9; Matt 15"
            ]
          )
        , ( 32
          , [ "Hosea 1-3; Matt 16"
            , "Hosea 4-6; Psalm 58; Matt 17"
            , "Hosea 7-10; Matt 18"
            , "Hosea 11-13; Matt 19"
            , "Hosea 14; 2 Chr 26-27; Ps 61; Matt 20"
            ]
          )
        , ( 33
          , [ "2 Kings 15-16; Matt 21"
            , "Isaiah 1-3; Psalm 9; Matt 22"
            , "Isaiah 4-6; Matt 23"
            , "Micah 1-4; Psalm 10; Matt 24"
            , "Micah 5-7; Matt 25"
            ]
          )
        , ( 34
          , [ "Isaiah 7-10; Psalm 22; Matt 26"
            , "Isa 11-13; Psalm 118; Matt 27"
            , "Isa 14-16; Matt 28"
            , "Isa 17-19; Psalm 62; 1 Cor 1"
            , "Isa 20-22; 1 Cor 2"
            ]
          )
        , ( 35
          , [ "Isaiah 23-25; 1 Cor 3"
            , "Isa 26-29; Psalm 65; 1 Cor 4"
            , "Isa 30-32; 1 Cor 5"
            , "Isa 33-35; 1 Cor 6"
            , "2 Chr 28; 2 Kings 17; Psalm 66; 1 Cor 7"
            ]
          )
        , ( 36
          , [ "2 Chr 29-31; 1 Cor 8"
            , "2 Kings 18-19; 2 Chr 32; Ps 67; 1 Cor 9"
            , "Isa 36-37; Psalm 123; 1 Cor 10"
            , "2 Kings 20; Isa 38-40; Ps 68; 1 Cor 11"
            , "Isa 41-44; 1 Cor 12"
            ]
          )
        , ( 37
          , [ "Isa 45-48; 1 Cor 13"
            , "Isa 49-52; Psalm 69; 1 Cor 14"
            , "Isa 53-55; Psalm 128; 1 Cor 15"
            , "Isa 56-59; Psalm 70; 1 Cor 16"
            , "Is 60-63; 2 Cor 1"
            ]
          )
        , ( 38
          , [ "Isa 64-66; 2 Cor 2"
            , "2 Kings 21; 2 Chr 33; Ps 71; 2 Cor 3"
            , "Nahum; Psalm 149; 2 Cor 4"
            , "2 Kings 22-23; Psalm 73; 2 Cor 5"
            , "2 Chr 34-35; 2 Cor 6"
            ]
          )
        , ( 39
          , [ "Habakkuk; 2 Cor 7"
            , "Zephaniah; Psalm 74; 2 Cor 8"
            , "Jeremiah 1-4; Psalm 130; 2 Cor 9"
            , "Jer 5-7; Psalm 75; 2 Cor 10"
            , "Jer 8-10; 2 Cor 11"
            ]
          )
        , ( 40
          , [ "Jer 11-13; 2 Cor 12"
            , "Jer 14-16; Psalm 76; 2 Cor 13"
            , "Jer 17-20; James 1"
            , "Jer 22, 23, 26; Psalm 77; James 2"
            , "Jer 25, 35, 36, 45; Ps 133; James 3"
            ]
          )
        , ( 41
          , [ "Jer 27, 28, 29, 24; James 4"
            , "Jer 37, 21, 34; Psalm 79; James 5"
            , "Jer 30-33; 1 Peter 1"
            , "Jer 38, 39, 52; 1 Pet 2"
            , "2 Kin 24-25; 2 Chr 36; Ps 126; 1 Pt 3"
            ]
          )
        , ( 42
          , [ "Lamentations; Psalm 137; 1 Peter 4"
            , "Obadiah; Jer 40-42; Ps 147; 1 Pet 5"
            , "Jer 43, 44, 46; 2 Pet 1"
            , "Jer 47, 48, 49; Ps 80; 2 Pet 2"
            , "Jer 50-51; 2 Pet 3"
            ]
          )
        , ( 43
          , [ "Ezekiel 1-3; John 1"
            , "Ezek 4-6; Psalm 82; John 2"
            , "Ezek 7-9; John 3"
            , "Ezek 10-12; Psalm 83; John 4"
            , "Ezek 13-15; Psalm 136; John 5"
            ]
          )
        , ( 44
          , [ "Ezekiel 16-18; John 6"
            , "Ezek 19-21; Psalm 84; John 7"
            , "Ezek 22-24; Psalm 134; John 8"
            , "Ezek 25-27; Psalm 85; John 9"
            , "Ezek 28-30; John 10"
            ]
          )
        , ( 45
          , [ "Ezekiel 31-33; John 11"
            , "Ezek 34-36; Psalm 86; John 12"
            , "Ezek 37-39; Psalm 87; John 13"
            , "Ezek 40-42; John 14"
            , "Ezek 43-45; Psalm 135; John 15"
            ]
          )
        , ( 46
          , [ "Ezekiel 46-48; John 16"
            , "Daniel 1-3; Psalm 88; John 17"
            , "Dan 4-6; John 18"
            , "Dan 7-9; Psalm 91; John 19"
            , "Dan 10-12; John 20"
            ]
          )
        , ( 47
          , [ "Ezra 1-2; John 21"
            , "Ezra 3-4; Psalm 92; 1 John 1"
            , "Haggai; Zechariah 1; Ps 138; 1 John 2"
            , "Zech 2-5; Psalm 93; 1 John 3"
            , "Zech 6-8; 1 John 4"
            ]
          )
        , ( 48
          , [ "Zech 9-11; 1 John 5"
            , "Zech 12-14; Psalm 94; 2 John"
            , "Ezra 5-6; Psalm 95; 3 John"
            , "Esther 1-3; Psalm 139; Revelation 1"
            , "Esther 4-6; Rev 2"
            ]
          )
        , ( 49
          , [ "Esther 7-10; Revelation 3"
            , "Ezra 7-10; Psalm 97; Rev 4"
            , "Nehemiah 1-3; Rev 5"
            , "Neh 4-6; Psalm 98; Rev 6"
            , "Neh 7-9; Psalm 140; Rev 7"
            ]
          )
        , ( 50
          , [ "Neh 10-13; Revelation 8"
            , "Malachi; Psalm 2; Rev 9"
            , "Job 1-3; Psalm 29; Rev 10"
            , "Job 4-7; Psalm 99; Rev 11"
            , "Job 8-11; Rev 12"
            ]
          )
        , ( 51
          , [ "Job 12-14; Psalm 100; Rev 13"
            , "Job 15-17; Rev 14"
            , "Job 18-20; Psalm 141; Rev 15"
            , "Job 21-23; Psalm 101; Rev 16"
            , "Job 24-27; Rev 17"
            ]
          )
        , ( 52
          , [ "Job 28-30; Revelation 18"
            , "Job 31-33; Psalm 102; Rev 19"
            , "Job 34-36; Rev 20"
            , "Job 37-39; Psalm 103; Rev 21"
            , "Job 40-42; Psalm 150; Rev 22"
            ]
          )
        ]


fiveDayPlanDataMachine : Dict Int (List (List Bible.Passage))
fiveDayPlanDataMachine =
    Dict.fromList
        [ ( 1
          , [ [ Bible.Passage (Bible.Ref Bible.Genesis 1) (Bible.Ref Bible.Genesis 2)
              , Bible.Passage (Bible.Ref Bible.Psalm 19) (Bible.Ref Bible.Psalm 19)
              , Bible.Passage (Bible.Ref Bible.Mark 1) (Bible.Ref Bible.Mark 1)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 3) (Bible.Ref Bible.Genesis 5)
              , Bible.Passage (Bible.Ref Bible.Mark 2) (Bible.Ref Bible.Mark 2)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 6) (Bible.Ref Bible.Genesis 8)
              , Bible.Passage (Bible.Ref Bible.Psalm 104) (Bible.Ref Bible.Psalm 104)
              , Bible.Passage (Bible.Ref Bible.Mark 3) (Bible.Ref Bible.Mark 3)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 9) (Bible.Ref Bible.Genesis 11)
              , Bible.Passage (Bible.Ref Bible.Mark 4) (Bible.Ref Bible.Mark 4)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 12) (Bible.Ref Bible.Genesis 15)
              , Bible.Passage (Bible.Ref Bible.Psalm 148) (Bible.Ref Bible.Psalm 148)
              , Bible.Passage (Bible.Ref Bible.Mark 5) (Bible.Ref Bible.Mark 5)
              ]
            ]
          )
        , ( 2
          , [ [ Bible.Passage (Bible.Ref Bible.Genesis 16) (Bible.Ref Bible.Genesis 18)
              , Bible.Passage (Bible.Ref Bible.Mark 6) (Bible.Ref Bible.Mark 6)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 19) (Bible.Ref Bible.Genesis 20)
              , Bible.Passage (Bible.Ref Bible.Psalm 1) (Bible.Ref Bible.Psalm 1)
              , Bible.Passage (Bible.Ref Bible.Mark 7) (Bible.Ref Bible.Mark 7)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 21) (Bible.Ref Bible.Genesis 23)
              , Bible.Passage (Bible.Ref Bible.Psalm 107) (Bible.Ref Bible.Psalm 107)
              , Bible.Passage (Bible.Ref Bible.Mark 8) (Bible.Ref Bible.Mark 8)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 24) (Bible.Ref Bible.Genesis 25)
              , Bible.Passage (Bible.Ref Bible.Psalm 4) (Bible.Ref Bible.Psalm 4)
              , Bible.Passage (Bible.Ref Bible.Mark 9) (Bible.Ref Bible.Mark 9)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 26) (Bible.Ref Bible.Genesis 27)
              , Bible.Passage (Bible.Ref Bible.Mark 10) (Bible.Ref Bible.Mark 10)
              ]
            ]
          )
        , ( 3
          , [ [ Bible.Passage (Bible.Ref Bible.Genesis 28) (Bible.Ref Bible.Genesis 29)
              , Bible.Passage (Bible.Ref Bible.Mark 11) (Bible.Ref Bible.Mark 11)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 30) (Bible.Ref Bible.Genesis 31)
              , Bible.Passage (Bible.Ref Bible.Psalm 11) (Bible.Ref Bible.Psalm 11)
              , Bible.Passage (Bible.Ref Bible.Mark 12) (Bible.Ref Bible.Mark 12)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 32) (Bible.Ref Bible.Genesis 34)
              , Bible.Passage (Bible.Ref Bible.Psalm 145) (Bible.Ref Bible.Psalm 145)
              , Bible.Passage (Bible.Ref Bible.Mark 13) (Bible.Ref Bible.Mark 13)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 35) (Bible.Ref Bible.Genesis 37)
              , Bible.Passage (Bible.Ref Bible.Psalm 12) (Bible.Ref Bible.Psalm 12)
              , Bible.Passage (Bible.Ref Bible.Mark 14) (Bible.Ref Bible.Mark 14)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 38) (Bible.Ref Bible.Genesis 40)
              , Bible.Passage (Bible.Ref Bible.Mark 15) (Bible.Ref Bible.Mark 15)
              ]
            ]
          )
        , ( 4
          , [ [ Bible.Passage (Bible.Ref Bible.Genesis 41) (Bible.Ref Bible.Genesis 42)
              , Bible.Passage (Bible.Ref Bible.Mark 16) (Bible.Ref Bible.Mark 16)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 43) (Bible.Ref Bible.Genesis 44)
              , Bible.Passage (Bible.Ref Bible.Psalm 24) (Bible.Ref Bible.Psalm 24)
              , Bible.Passage (Bible.Ref Bible.Galatians 1) (Bible.Ref Bible.Galatians 1)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 45) (Bible.Ref Bible.Genesis 46)
              , Bible.Passage (Bible.Ref Bible.Psalm 108) (Bible.Ref Bible.Psalm 108)
              , Bible.Passage (Bible.Ref Bible.Galatians 2) (Bible.Ref Bible.Galatians 2)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 47) (Bible.Ref Bible.Genesis 48)
              , Bible.Passage (Bible.Ref Bible.Psalm 25) (Bible.Ref Bible.Psalm 25)
              , Bible.Passage (Bible.Ref Bible.Galatians 3) (Bible.Ref Bible.Galatians 3)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Genesis 49) (Bible.Ref Bible.Genesis 50)
              , Bible.Passage (Bible.Ref Bible.Galatians 4) (Bible.Ref Bible.Galatians 4)
              ]
            ]
          )
        , ( 5
          , [ [ Bible.Passage (Bible.Ref Bible.Exodus 1) (Bible.Ref Bible.Exodus 3)
              , Bible.Passage (Bible.Ref Bible.Galatians 5) (Bible.Ref Bible.Galatians 5)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 4) (Bible.Ref Bible.Exodus 6)
              , Bible.Passage (Bible.Ref Bible.Galatians 6) (Bible.Ref Bible.Galatians 6)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 7) (Bible.Ref Bible.Exodus 9)
              , Bible.Passage (Bible.Ref Bible.Psalm 105) (Bible.Ref Bible.Psalm 105)
              , Bible.Passage (Bible.Ref Bible.Ephesians 1) (Bible.Ref Bible.Ephesians 1)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 10) (Bible.Ref Bible.Exodus 12)
              , Bible.Passage (Bible.Ref Bible.Ephesians 2) (Bible.Ref Bible.Ephesians 2)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 13) (Bible.Ref Bible.Exodus 15)
              , Bible.Passage (Bible.Ref Bible.Psalm 114) (Bible.Ref Bible.Psalm 114)
              , Bible.Passage (Bible.Ref Bible.Ephesians 3) (Bible.Ref Bible.Ephesians 3)
              ]
            ]
          )
        , ( 6
          , [ [ Bible.Passage (Bible.Ref Bible.Exodus 16) (Bible.Ref Bible.Exodus 18)
              , Bible.Passage (Bible.Ref Bible.Ephesians 4) (Bible.Ref Bible.Ephesians 4)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 19) (Bible.Ref Bible.Exodus 21)
              , Bible.Passage (Bible.Ref Bible.Psalm 33) (Bible.Ref Bible.Psalm 33)
              , Bible.Passage (Bible.Ref Bible.Ephesians 5) (Bible.Ref Bible.Ephesians 5)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 22) (Bible.Ref Bible.Exodus 24)
              , Bible.Passage (Bible.Ref Bible.Psalm 109) (Bible.Ref Bible.Psalm 109)
              , Bible.Passage (Bible.Ref Bible.Ephesians 6) (Bible.Ref Bible.Ephesians 6)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 25) (Bible.Ref Bible.Exodus 27)
              , Bible.Passage (Bible.Ref Bible.Psalm 90) (Bible.Ref Bible.Psalm 90)
              , Bible.Passage (Bible.Ref Bible.Philippians 1) (Bible.Ref Bible.Philippians 1)
              ]
            , [ Bible.Passage (Bible.Ref Bible.Exodus 28) (Bible.Ref Bible.Exodus 31)
              , Bible.Passage (Bible.Ref Bible.Philippians 2) (Bible.Ref Bible.Philippians 2)
              ]
            ]
          )
        ]



{- , ( 7
   --   , [ "Exodus 32-34; Philippians 3"
   --     , "Exodus 35-37; Psalm 26; Phil 4"
   --     , "Exodus 38-40; Hebrews 1"
   --     , "Leviticus 1-3; Psalm 27; Heb 2"
   --     , "Lev 4-7; Heb 3"
   --     ]
   --   )
   -- , ( 8
   --   , [ "Leviticus 8-11; Ps 110; Hebrews 4"
   --     , "Lev 12-14; Psalm 111; Heb 5"
   --     , "Lev 15-18; Psalm 31; Heb 6"
   --     , "Lev 19-20; Heb 7"
   --     , "Lev 21-23; Heb 8"
   --     ]
   --   )
   -- , ( 9
   --   , [ "Leviticus 24-25; Psalm 81; Hebrews 9"
   --     , "Lev 26-27; Psalm 112; Heb 10"
   --     , "Numbers 1-2; Psalm 64; Heb 11"
   --     , "Num 3-5; Heb 12"
   --     , "Num 6-7; Heb 13"
   --     ]
   --   )
   -- , ( 10
   --   , [ "Numbers 8-11; Colossians 1"
   --     , "Num 12-14; Psalm 28; Col 2"
   --     , "Num 15-18; Psalm 113; Col 3"
   --     , "Num 19-21; Col 4"
   --     , "Num 22-25; Luke 1"
   --     ]
   --   )
   -- , ( 11
   --   , [ "Numbers 26-29; Luke 2"
   --     , "Num 30-33; Psalm 35; Luke 3"
   --     , "Num 34-36; Luke 4"
   --     , "Deuteronomy 1-3; Psalm 36; Luke 5"
   --     , "Deut 4-5; Luke 6"
   --     ]
   --   )
   -- , ( 12
   --   , [ "Deuteronomy 6-9; Luke 7"
   --     , "Deut 10-14; Psalm 5; Luke 8"
   --     , "Deut 15-18; Psalm 115; Luke 9"
   --     , "Deut 19-22; Psalm 6; Luke 10"
   --     , "Deut 23-26; Luke 11"
   --     ]
   --   )
   -- , ( 13
   --   , [ "Deut 27-31; Luke 12"
   --     , "Deut 32-34; Psalm 13; Luke 13"
   --     , "Joshua 1-4; Psalm 143; Luke 14"
   --     , "Joshua 5-8; Psalm 14; Luke 15"
   --     , "Joshua 9-13; Luke 16"
   --     ]
   --   )
   -- , ( 14
   --   , [ "Joshua 14-17; Luke 17"
   --     , "Joshua 18-21; Psalm 15; Luke 18"
   --     , "Joshua 22-24; Psalm 116; Luke 19"
   --     , "Judges 1-3; Psalm 16; Luke 20"
   --     , "Judges 4-6; Luke 21"
   --     ]
   --   )
   -- , ( 15
   --   , [ "Judges 7-8; Luke 22"
   --     , "Judges 9-11; Psalm 17; Luke 23"
   --     , "Judges 12-16; Psalm 146; Luke 24"
   --     , "Judges 17-18; Psalm 21; Acts 1"
   --     , "Judges 19-21; Acts 2"
   --     ]
   --   )
   -- , ( 16
   --   , [ "Ruth 1-2; Acts 3"
   --     , "Ruth 3-4; Psalm 37; Acts 4"
   --     , "1 Samuel 1-2; Psalm 120; Acts 5"
   --     , "1 Sam 3-5; Psalm 23; Acts 6"
   --     , "1 Sam 6-8; Acts 7"
   --     ]
   --   )
   -- , ( 17
   --   , [ "1 Samuel 9-10; Acts 8"
   --     , "1 Sam 11-13; Psalm 38; Acts 9"
   --     , "1 Sam 14; Psalm 124; Acts 10"
   --     , "1 Sam 15-16; 1 Chr 1; Ps 39; Acts 11"
   --     , "1 Sam 17; 1 Chr 2; Acts 12"
   --     ]
   --   )
   -- , ( 18
   --   , [ "1 Sam 18-19; 1 Chr 3; Ps 59; Acts 13"
   --     , "1 Sa 20;1 Chr 4; Ps 56, 57, 142; Acts 14"
   --     , "1 Sam 21-22; 1 Chr 5; Ps 52; Acts 15"
   --     , "1 Sam 23-24; 1 Chr 6; Ps 54; Acts 16"
   --     , "1 Sam 25; 1 Chr 7; Acts 17"
   --     ]
   --   )
   -- , ( 19
   --   , [ "1 Sam 26-27; 1 Chr 8; Acts 18"
   --     , "1 Sam 28-29; 1 Chr 9; Acts 19"
   --     , "1 Sam 30-31; 1 Chr 10; Acts 20"
   --     , "2 Sam 1-2; 1 Chr 11; Ps 96, 106; Acts 21"
   --     , "2 Sam 3-5; 1 Chr 12; Ps 122; Acts 22"
   --     ]
   --   )
   -- , ( 20
   --   , [ "2 Sam 6; 1 Chr 13; Psalm 60; Acts 23"
   --     , "1 Chron 14-16; Acts 24"
   --     , "2 Sam 7-8; 1 Chr 17; Ps 132; Acts 25"
   --     , "2 Sam 9-10; 1 Chr 18-19; Ps 89; Acts 26"
   --     , "2 Sa 11-12; 1 Chr 20; Ps 51, 32; Acts 27"
   --     ]
   --   )
   -- , ( 21
   --   , [ "2 Sam 13-14; Acts 28"
   --     , "2 Sam 15-17; Psalms 3, 63; Romans 1"
   --     , "2 Sam 18-20; Psalm 34; Romans 2"
   --     , "2 Sam 21-23; Psalm 18; Romans 3"
   --     , "2 Sam 24; 1 Chr 21; Romans 4"
   --     ]
   --   )
   -- , ( 22
   --   , [ "1 Chr 22-25; Psalm 78; Romans 5"
   --     , "1 Kings 1; 1 Chr 26-28; Romans 6"
   --     , "1 Kings 2; 1 Chr 29; Romans 7"
   --     , "1 Kings 3; 2 Chr 1; Ps 42; Romans 8"
   --     , "1 Kings 4; Prov 1-2; Psalm 43; Romans 9"
   --     ]
   --   )
   -- , ( 23
   --   , [ "Proverbs 3-5; Romans 10"
   --     , "Proverbs 6-7; Psalm 7; Romans 11"
   --     , "Proverbs 8-10; Psalm 144; Romans 12"
   --     , "Proverbs 11-13; Ps 8; Romans 13"
   --     , "Proverbs 14-15; Romans 14"
   --     ]
   --   )
   -- , ( 24
   --   , [ "Proverbs 16-18; Romans 15"
   --     , "Proverbs 19-21; Ps 40; Romans 16"
   --     , "Proverbs 22-23; Ps 117; 1 Thess 1"
   --     , "Proverbs 24-25; Ps 41; 1 Thess 2"
   --     , "Proverbs 26-28; 1 Thess 3"
   --     ]
   --   )
   -- , ( 25
   --   , [ "Proverbs 29-31; 1 Thess 4"
   --     , "Song of Sol 1-3; Ps 72; 1 Thess 5"
   --     , "Song of Sol 4-6; 2 Thess 1"
   --     , "Song of Sol 7-8; Psalm 127; 2 Thess 2"
   --     , "1 Kings 5; 2 Chr 2; 2 Thess 3"
   --     ]
   --   )
   -- , ( 26
   --   , [ "1 Kings 6; 2 Chr 3; 1 Timothy 1"
   --     , "1 Kings 7; 2 Chr 4; Psalm 44; 1 Tim 2"
   --     , "1 Kings 8; Psalm 30; 1 Tim 3"
   --     , "2 Chr 5-7; Psalm 121; 1 Tim 4"
   --     , "1 Kings 9; 2 Chr 8; 1 Tim 5"
   --     ]
   --   )
   -- , ( 27
   --   , [ "1 Kings 10-11; 2 Chr 9; 1 Tim 6"
   --     , "Ecclesiastes 1-3; Psalm 45; 2 Tim 1"
   --     , "Eccl 4-6; Psalm 125; 2 Tim 2"
   --     , "Eccl 7-9; Psalm 46; 2 Tim 3"
   --     , "Eccl 10-12; 2 Tim 4"
   --     ]
   --   )
   -- , ( 28
   --   , [ "1 Kings 12; 2 Chr 10-11; Titus 1"
   --     , "1 Kings 13-14; 2 Chr 12; Ps 47; Titus 2"
   --     , "1 Kings 15; 2 Chr 13-14; Titus 3"
   --     , "2 Chr 15-16; 1 Kings 16; Philemon"
   --     , "1 Kings 17-18; Psalm 119; Jude"
   --     ]
   --   )
   -- , ( 29
   --   , [ "1 Kin 19-21; 2 Chr 17; Ps 129; Matt 1"
   --     , "1 Kings 22; 2 Chr 18; Matt 2"
   --     , "2 Chr 19-20; 2 Kings 1; Psalm 20; Matt 3"
   --     , "2 Kings 2-3; Psalm 48; Matt 4"
   --     , "2 Kings 4-6; Matt 5"
   --     ]
   --   )
   -- , ( 30
   --   , [ "2 Kings 7-8; 2 Chr 21; Matt 6"
   --     , "2 Kings 9-10; Psalm 49; Matt 7"
   --     , "2 Chr 22-23; 2 Kings 11; Ps 131; Matt 8"
   --     , "2 Chr 24; 2 Kings 12; Psalm 50; Matt 9"
   --     , "Joel; Matt 10"
   --     ]
   --   )
   -- , ( 31
   --   , [ "Jonah; Matt 11"
   --     , "2 Kings 13-14; 2 Chr 25; Ps 53; Matt 12"
   --     , "Amos 1-3; Matt 13"
   --     , "Amos 4-6; Psalm 55; Matt 14"
   --     , "Amos 7-9; Matt 15"
   --     ]
   --   )
   -- , ( 32
   --   , [ "Hosea 1-3; Matt 16"
   --     , "Hosea 4-6; Psalm 58; Matt 17"
   --     , "Hosea 7-10; Matt 18"
   --     , "Hosea 11-13; Matt 19"
   --     , "Hosea 14; 2 Chr 26-27; Ps 61; Matt 20"
   --     ]
   --   )
   -- , ( 33
   --   , [ "2 Kings 15-16; Matt 21"
   --     , "Isaiah 1-3; Psalm 9; Matt 22"
   --     , "Isaiah 4-6; Matt 23"
   --     , "Micah 1-4; Psalm 10; Matt 24"
   --     , "Micah 5-7; Matt 25"
   --     ]
   --   )
   -- , ( 34
   --   , [ "Isaiah 7-10; Psalm 22; Matt 26"
   --     , "Isa 11-13; Psalm 118; Matt 27"
   --     , "Isa 14-16; Matt 28"
   --     , "Isa 17-19; Psalm 62; 1 Cor 1"
   --     , "Isa 20-22; 1 Cor 2"
   --     ]
   --   )
   -- , ( 35
   --   , [ "Isaiah 23-25; 1 Cor 3"
   --     , "Isa 26-29; Psalm 65; 1 Cor 4"
   --     , "Isa 30-32; 1 Cor 5"
   --     , "Isa 33-35; 1 Cor 6"
   --     , "2 Chr 28; 2 Kings 17; Psalm 66; 1 Cor 7"
   --     ]
   --   )
   -- , ( 36
   --   , [ "2 Chr 29-31; 1 Cor 8"
   --     , "2 Kings 18-19; 2 Chr 32; Ps 67; 1 Cor 9"
   --     , "Isa 36-37; Psalm 123; 1 Cor 10"
   --     , "2 Kings 20; Isa 38-40; Ps 68; 1 Cor 11"
   --     , "Isa 41-44; 1 Cor 12"
   --     ]
   --   )
   -- , ( 37
   --   , [ "Isa 45-48; 1 Cor 13"
   --     , "Isa 49-52; Psalm 69; 1 Cor 14"
   --     , "Isa 53-55; Psalm 128; 1 Cor 15"
   --     , "Isa 56-59; Psalm 70; 1 Cor 16"
   --     , "Is 60-63; 2 Cor 1"
   --     ]
   --   )
   -- , ( 38
   --   , [ "Isa 64-66; 2 Cor 2"
   --     , "2 Kings 21; 2 Chr 33; Ps 71; 2 Cor 3"
   --     , "Nahum; Psalm 149; 2 Cor 4"
   --     , "2 Kings 22-23; Psalm 73; 2 Cor 5"
   --     , "2 Chr 34-35; 2 Cor 6"
   --     ]
   --   )
   -- , ( 39
   --   , [ "Habakkuk; 2 Cor 7"
   --     , "Zephaniah; Psalm 74; 2 Cor 8"
   --     , "Jeremiah 1-4; Psalm 130; 2 Cor 9"
   --     , "Jer 5-7; Psalm 75; 2 Cor 10"
   --     , "Jer 8-10; 2 Cor 11"
   --     ]
   --   )
   -- , ( 40
   --   , [ "Jer 11-13; 2 Cor 12"
   --     , "Jer 14-16; Psalm 76; 2 Cor 13"
   --     , "Jer 17-20; James 1"
   --     , "Jer 22, 23, 26; Psalm 77; James 2"
   --     , "Jer 25, 35, 36, 45; Ps 133; James 3"
   --     ]
   --   )
   -- , ( 41
   --   , [ "Jer 27, 28, 29, 24; James 4"
   --     , "Jer 37, 21, 34; Psalm 79; James 5"
   --     , "Jer 30-33; 1 Peter 1"
   --     , "Jer 38, 39, 52; 1 Pet 2"
   --     , "2 Kin 24-25; 2 Chr 36; Ps 126; 1 Pt 3"
   --     ]
   --   )
   -- , ( 42
   --   , [ "Lamentations; Psalm 137; 1 Peter 4"
   --     , "Obadiah; Jer 40-42; Ps 147; 1 Pet 5"
   --     , "Jer 43, 44, 46; 2 Pet 1"
   --     , "Jer 47, 48, 49; Ps 80; 2 Pet 2"
   --     , "Jer 50-51; 2 Pet 3"
   --     ]
   --   )
   -- , ( 43
   --   , [ "Ezekiel 1-3; John 1"
   --     , "Ezek 4-6; Psalm 82; John 2"
   --     , "Ezek 7-9; John 3"
   --     , "Ezek 10-12; Psalm 83; John 4"
   --     , "Ezek 13-15; Psalm 136; John 5"
   --     ]
   --   )
   -- , ( 44
   --   , [ "Ezekiel 16-18; John 6"
   --     , "Ezek 19-21; Psalm 84; John 7"
   --     , "Ezek 22-24; Psalm 134; John 8"
   --     , "Ezek 25-27; Psalm 85; John 9"
   --     , "Ezek 28-30; John 10"
   --     ]
   --   )
   -- , ( 45
   --   , [ "Ezekiel 31-33; John 11"
   --     , "Ezek 34-36; Psalm 86; John 12"
   --     , "Ezek 37-39; Psalm 87; John 13"
   --     , "Ezek 40-42; John 14"
   --     , "Ezek 43-45; Psalm 135; John 15"
   --     ]
   --   )
   -- , ( 46
   --   , [ "Ezekiel 46-48; John 16"
   --     , "Daniel 1-3; Psalm 88; John 17"
   --     , "Dan 4-6; John 18"
   --     , "Dan 7-9; Psalm 91; John 19"
   --     , "Dan 10-12; John 20"
   --     ]
   --   )
   -- , ( 47
   --   , [ "Ezra 1-2; John 21"
   --     , "Ezra 3-4; Psalm 92; 1 John 1"
   --     , "Haggai; Zechariah 1; Ps 138; 1 John 2"
   --     , "Zech 2-5; Psalm 93; 1 John 3"
   --     , "Zech 6-8; 1 John 4"
   --     ]
   --   )
   -- , ( 48
   --   , [ "Zech 9-11; 1 John 5"
   --     , "Zech 12-14; Psalm 94; 2 John"
   --     , "Ezra 5-6; Psalm 95; 3 John"
   --     , "Esther 1-3; Psalm 139; Revelation 1"
   --     , "Esther 4-6; Rev 2"
   --     ]
   --   )
   -- , ( 49
   --   , [ "Esther 7-10; Revelation 3"
   --     , "Ezra 7-10; Psalm 97; Rev 4"
   --     , "Nehemiah 1-3; Rev 5"
   --     , "Neh 4-6; Psalm 98; Rev 6"
   --     , "Neh 7-9; Psalm 140; Rev 7"
   --     ]
   --   )
   -- , ( 50
   --   , [ "Neh 10-13; Revelation 8"
   --     , "Malachi; Psalm 2; Rev 9"
   --     , "Job 1-3; Psalm 29; Rev 10"
   --     , "Job 4-7; Psalm 99; Rev 11"
   --     , "Job 8-11; Rev 12"
   --     ]
   --   )
   -- , ( 51
   --   , [ "Job 12-14; Psalm 100; Rev 13"
   --     , "Job 15-17; Rev 14"
   --     , "Job 18-20; Psalm 141; Rev 15"
   --     , "Job 21-23; Psalm 101; Rev 16"
   --     , "Job 24-27; Rev 17"
   --     ]
   --   )
   -- , ( 52
   --   , [ "Job 28-30; Revelation 18"
   --     , "Job 31-33; Psalm 102; Rev 19"
   --     , "Job 34-36; Rev 20"
   --     , "Job 37-39; Psalm 103; Rev 21"
   --     , "Job 40-42; Psalm 150; Rev 22"
   --     ]
   --   )
   -- ]
-}


dateToWeekNum : Dict ( Int, Int ) Int
dateToWeekNum =
    Dict.fromList
        [ ( ( 1, 5 ), 1 )
        , ( ( 1, 6 ), 1 )
        , ( ( 1, 7 ), 1 )
        , ( ( 1, 8 ), 1 )
        , ( ( 1, 9 ), 1 )
        , ( ( 1, 10 ), 1 )
        , ( ( 1, 11 ), 1 )
        , ( ( 1, 12 ), 2 )
        , ( ( 1, 13 ), 2 )
        , ( ( 1, 14 ), 2 )
        , ( ( 1, 15 ), 2 )
        , ( ( 1, 16 ), 2 )
        , ( ( 1, 17 ), 2 )
        , ( ( 1, 18 ), 2 )
        , ( ( 1, 19 ), 3 )
        , ( ( 1, 20 ), 3 )
        , ( ( 1, 21 ), 3 )
        , ( ( 1, 22 ), 3 )
        , ( ( 1, 23 ), 3 )
        , ( ( 1, 24 ), 3 )
        , ( ( 1, 25 ), 3 )
        , ( ( 1, 26 ), 4 )
        , ( ( 1, 27 ), 4 )
        , ( ( 1, 28 ), 4 )
        , ( ( 1, 29 ), 4 )
        , ( ( 1, 30 ), 4 )
        , ( ( 1, 31 ), 4 )
        , ( ( 2, 1 ), 4 )
        , ( ( 2, 2 ), 5 )
        , ( ( 2, 3 ), 5 )
        , ( ( 2, 4 ), 5 )
        , ( ( 2, 5 ), 5 )
        , ( ( 2, 6 ), 5 )
        , ( ( 2, 7 ), 5 )
        , ( ( 2, 8 ), 5 )
        , ( ( 2, 9 ), 6 )
        , ( ( 2, 10 ), 6 )
        , ( ( 2, 11 ), 6 )
        , ( ( 2, 12 ), 6 )
        , ( ( 2, 13 ), 6 )
        , ( ( 2, 14 ), 6 )
        , ( ( 2, 15 ), 6 )
        , ( ( 2, 16 ), 67 )
        , ( ( 2, 17 ), 7 )
        , ( ( 2, 18 ), 7 )
        , ( ( 2, 19 ), 7 )
        , ( ( 2, 20 ), 7 )
        , ( ( 2, 21 ), 7 )
        , ( ( 2, 22 ), 7 )
        , ( ( 2, 23 ), 8 )
        , ( ( 2, 24 ), 8 )
        , ( ( 2, 25 ), 8 )
        , ( ( 2, 26 ), 8 )
        , ( ( 2, 27 ), 8 )
        , ( ( 2, 28 ), 8 )
        , ( ( 2, 29 ), 8 )
        , ( ( 3, 1 ), 9 )
        , ( ( 3, 2 ), 9 )
        , ( ( 3, 3 ), 9 )
        , ( ( 3, 4 ), 9 )
        , ( ( 3, 5 ), 9 )
        , ( ( 3, 6 ), 9 )
        , ( ( 3, 7 ), 9 )
        , ( ( 3, 8 ), 10 )
        , ( ( 3, 9 ), 10 )
        , ( ( 3, 10 ), 10 )
        , ( ( 3, 11 ), 10 )
        , ( ( 3, 12 ), 10 )
        , ( ( 3, 13 ), 10 )
        , ( ( 3, 14 ), 10 )
        , ( ( 3, 15 ), 11 )
        , ( ( 3, 16 ), 11 )
        , ( ( 3, 17 ), 11 )
        , ( ( 3, 18 ), 11 )
        , ( ( 3, 19 ), 11 )
        , ( ( 3, 20 ), 11 )
        , ( ( 3, 21 ), 11 )
        , ( ( 3, 22 ), 12 )
        , ( ( 3, 23 ), 12 )
        , ( ( 3, 24 ), 12 )
        , ( ( 3, 25 ), 12 )
        , ( ( 3, 26 ), 12 )
        , ( ( 3, 27 ), 12 )
        , ( ( 3, 28 ), 12 )
        , ( ( 3, 29 ), 13 )
        , ( ( 3, 30 ), 13 )
        , ( ( 3, 31 ), 13 )
        , ( ( 4, 1 ), 13 )
        , ( ( 4, 2 ), 13 )
        , ( ( 4, 3 ), 13 )
        , ( ( 4, 4 ), 13 )
        , ( ( 4, 5 ), 14 )
        , ( ( 4, 6 ), 14 )
        , ( ( 4, 7 ), 14 )
        , ( ( 4, 8 ), 14 )
        , ( ( 4, 9 ), 14 )
        , ( ( 4, 10 ), 14 )
        , ( ( 4, 11 ), 14 )
        , ( ( 4, 12 ), 15 )
        , ( ( 4, 13 ), 15 )
        , ( ( 4, 14 ), 15 )
        , ( ( 4, 15 ), 15 )
        , ( ( 4, 16 ), 15 )
        , ( ( 4, 17 ), 15 )
        , ( ( 4, 18 ), 15 )
        , ( ( 4, 19 ), 16 )
        , ( ( 4, 20 ), 16 )
        , ( ( 4, 21 ), 16 )
        , ( ( 4, 22 ), 16 )
        , ( ( 4, 23 ), 16 )
        , ( ( 4, 24 ), 16 )
        , ( ( 4, 25 ), 16 )
        , ( ( 4, 20 ), 17 )
        , ( ( 4, 21 ), 17 )
        , ( ( 4, 22 ), 17 )
        , ( ( 4, 23 ), 17 )
        , ( ( 4, 24 ), 17 )
        , ( ( 4, 25 ), 17 )
        , ( ( 4, 26 ), 17 )
        , ( ( 4, 27 ), 17 )
        , ( ( 4, 28 ), 17 )
        , ( ( 4, 29 ), 17 )
        , ( ( 4, 30 ), 17 )
        , ( ( 5, 1 ), 17 )
        , ( ( 5, 2 ), 17 )
        , ( ( 5, 3 ), 18 )
        , ( ( 5, 4 ), 18 )
        , ( ( 5, 5 ), 18 )
        , ( ( 5, 6 ), 18 )
        , ( ( 5, 7 ), 18 )
        , ( ( 5, 8 ), 18 )
        , ( ( 5, 9 ), 18 )
        , ( ( 5, 10 ), 19 )
        , ( ( 5, 11 ), 19 )
        , ( ( 5, 12 ), 19 )
        , ( ( 5, 13 ), 19 )
        , ( ( 5, 14 ), 19 )
        , ( ( 5, 15 ), 19 )
        , ( ( 5, 16 ), 19 )
        , ( ( 5, 17 ), 20 )
        , ( ( 5, 18 ), 20 )
        , ( ( 5, 19 ), 20 )
        , ( ( 5, 20 ), 20 )
        , ( ( 5, 21 ), 20 )
        , ( ( 5, 22 ), 20 )
        , ( ( 5, 23 ), 20 )
        , ( ( 5, 24 ), 21 )
        , ( ( 5, 25 ), 21 )
        , ( ( 5, 26 ), 21 )
        , ( ( 5, 27 ), 21 )
        , ( ( 5, 28 ), 21 )
        , ( ( 5, 29 ), 21 )
        , ( ( 5, 30 ), 21 )
        , ( ( 5, 31 ), 22 )
        , ( ( 6, 1 ), 22 )
        , ( ( 6, 2 ), 22 )
        , ( ( 6, 3 ), 22 )
        , ( ( 6, 4 ), 22 )
        , ( ( 6, 5 ), 22 )
        , ( ( 6, 6 ), 22 )
        , ( ( 6, 7 ), 23 )
        , ( ( 6, 8 ), 23 )
        , ( ( 6, 9 ), 23 )
        , ( ( 6, 10 ), 23 )
        , ( ( 6, 11 ), 23 )
        , ( ( 6, 12 ), 23 )
        , ( ( 6, 13 ), 23 )
        , ( ( 6, 14 ), 24 )
        , ( ( 6, 15 ), 24 )
        , ( ( 6, 16 ), 24 )
        , ( ( 6, 17 ), 24 )
        , ( ( 6, 18 ), 24 )
        , ( ( 6, 19 ), 24 )
        , ( ( 6, 20 ), 24 )
        , ( ( 6, 21 ), 25 )
        , ( ( 6, 22 ), 25 )
        , ( ( 6, 23 ), 25 )
        , ( ( 6, 24 ), 25 )
        , ( ( 6, 25 ), 25 )
        , ( ( 6, 26 ), 25 )
        , ( ( 6, 27 ), 25 )
        , ( ( 6, 28 ), 26 )
        , ( ( 6, 29 ), 26 )
        , ( ( 6, 30 ), 26 )
        , ( ( 7, 1 ), 26 )
        , ( ( 7, 2 ), 26 )
        , ( ( 7, 3 ), 26 )
        , ( ( 7, 4 ), 26 )
        , ( ( 7, 5 ), 27 )
        , ( ( 7, 6 ), 27 )
        , ( ( 7, 7 ), 27 )
        , ( ( 7, 8 ), 27 )
        , ( ( 7, 9 ), 27 )
        , ( ( 7, 10 ), 27 )
        , ( ( 7, 11 ), 27 )
        , ( ( 7, 12 ), 28 )
        , ( ( 7, 13 ), 28 )
        , ( ( 7, 14 ), 28 )
        , ( ( 7, 15 ), 28 )
        , ( ( 7, 16 ), 28 )
        , ( ( 7, 17 ), 28 )
        , ( ( 7, 18 ), 28 )
        , ( ( 7, 19 ), 29 )
        , ( ( 7, 20 ), 29 )
        , ( ( 7, 21 ), 29 )
        , ( ( 7, 22 ), 29 )
        , ( ( 7, 23 ), 29 )
        , ( ( 7, 24 ), 29 )
        , ( ( 7, 25 ), 29 )
        , ( ( 7, 26 ), 30 )
        , ( ( 7, 27 ), 30 )
        , ( ( 7, 28 ), 30 )
        , ( ( 7, 29 ), 30 )
        , ( ( 7, 30 ), 30 )
        , ( ( 7, 31 ), 30 )
        , ( ( 8, 1 ), 30 )
        , ( ( 8, 2 ), 31 )
        , ( ( 8, 3 ), 31 )
        , ( ( 8, 4 ), 31 )
        , ( ( 8, 5 ), 31 )
        , ( ( 8, 6 ), 31 )
        , ( ( 8, 7 ), 31 )
        , ( ( 8, 8 ), 31 )
        , ( ( 8, 9 ), 32 )
        , ( ( 8, 10 ), 32 )
        , ( ( 8, 11 ), 32 )
        , ( ( 8, 12 ), 32 )
        , ( ( 8, 13 ), 32 )
        , ( ( 8, 14 ), 32 )
        , ( ( 8, 15 ), 32 )
        , ( ( 8, 16 ), 33 )
        , ( ( 8, 17 ), 33 )
        , ( ( 8, 18 ), 33 )
        , ( ( 8, 19 ), 33 )
        , ( ( 8, 20 ), 33 )
        , ( ( 8, 21 ), 33 )
        , ( ( 8, 22 ), 33 )
        , ( ( 8, 23 ), 34 )
        , ( ( 8, 24 ), 34 )
        , ( ( 8, 25 ), 34 )
        , ( ( 8, 26 ), 34 )
        , ( ( 8, 27 ), 34 )
        , ( ( 8, 28 ), 34 )
        , ( ( 8, 29 ), 34 )
        , ( ( 8, 30 ), 35 )
        , ( ( 8, 31 ), 35 )
        , ( ( 9, 1 ), 35 )
        , ( ( 9, 2 ), 35 )
        , ( ( 9, 3 ), 35 )
        , ( ( 9, 4 ), 35 )
        , ( ( 9, 5 ), 35 )
        , ( ( 9, 6 ), 36 )
        , ( ( 9, 7 ), 36 )
        , ( ( 9, 8 ), 36 )
        , ( ( 9, 9 ), 36 )
        , ( ( 9, 10 ), 36 )
        , ( ( 9, 11 ), 36 )
        , ( ( 9, 12 ), 36 )
        , ( ( 9, 13 ), 37 )
        , ( ( 9, 14 ), 37 )
        , ( ( 9, 15 ), 37 )
        , ( ( 9, 16 ), 37 )
        , ( ( 9, 17 ), 37 )
        , ( ( 9, 18 ), 37 )
        , ( ( 9, 19 ), 37 )
        , ( ( 9, 20 ), 38 )
        , ( ( 9, 21 ), 38 )
        , ( ( 9, 22 ), 38 )
        , ( ( 9, 23 ), 38 )
        , ( ( 9, 24 ), 38 )
        , ( ( 9, 25 ), 38 )
        , ( ( 9, 26 ), 38 )
        , ( ( 9, 27 ), 39 )
        , ( ( 9, 28 ), 39 )
        , ( ( 9, 29 ), 39 )
        , ( ( 9, 30 ), 39 )
        , ( ( 10, 1 ), 39 )
        , ( ( 10, 2 ), 39 )
        , ( ( 10, 3 ), 39 )
        , ( ( 10, 4 ), 40 )
        , ( ( 10, 5 ), 40 )
        , ( ( 10, 6 ), 40 )
        , ( ( 10, 7 ), 40 )
        , ( ( 10, 8 ), 40 )
        , ( ( 10, 9 ), 40 )
        , ( ( 10, 10 ), 40 )
        , ( ( 10, 11 ), 41 )
        , ( ( 10, 12 ), 41 )
        , ( ( 10, 13 ), 41 )
        , ( ( 10, 14 ), 41 )
        , ( ( 10, 15 ), 41 )
        , ( ( 10, 16 ), 41 )
        , ( ( 10, 17 ), 41 )
        , ( ( 10, 18 ), 42 )
        , ( ( 10, 19 ), 42 )
        , ( ( 10, 20 ), 42 )
        , ( ( 10, 21 ), 42 )
        , ( ( 10, 22 ), 42 )
        , ( ( 10, 23 ), 42 )
        , ( ( 10, 24 ), 42 )
        , ( ( 10, 25 ), 43 )
        , ( ( 10, 26 ), 43 )
        , ( ( 10, 27 ), 43 )
        , ( ( 10, 28 ), 43 )
        , ( ( 10, 29 ), 43 )
        , ( ( 10, 30 ), 43 )
        , ( ( 10, 31 ), 43 )
        , ( ( 11, 1 ), 44 )
        , ( ( 11, 2 ), 44 )
        , ( ( 11, 3 ), 44 )
        , ( ( 11, 4 ), 44 )
        , ( ( 11, 5 ), 44 )
        , ( ( 11, 6 ), 44 )
        , ( ( 11, 7 ), 44 )
        , ( ( 11, 8 ), 45 )
        , ( ( 11, 9 ), 45 )
        , ( ( 11, 10 ), 45 )
        , ( ( 11, 11 ), 45 )
        , ( ( 11, 12 ), 45 )
        , ( ( 11, 13 ), 45 )
        , ( ( 11, 14 ), 45 )
        , ( ( 11, 15 ), 46 )
        , ( ( 11, 16 ), 46 )
        , ( ( 11, 17 ), 46 )
        , ( ( 11, 18 ), 46 )
        , ( ( 11, 19 ), 46 )
        , ( ( 11, 20 ), 46 )
        , ( ( 11, 21 ), 46 )
        , ( ( 11, 22 ), 47 )
        , ( ( 11, 23 ), 47 )
        , ( ( 11, 24 ), 47 )
        , ( ( 11, 25 ), 47 )
        , ( ( 11, 26 ), 47 )
        , ( ( 11, 27 ), 47 )
        , ( ( 11, 28 ), 47 )
        , ( ( 11, 29 ), 48 )
        , ( ( 11, 30 ), 48 )
        , ( ( 12, 1 ), 48 )
        , ( ( 12, 2 ), 48 )
        , ( ( 12, 3 ), 48 )
        , ( ( 12, 4 ), 48 )
        , ( ( 12, 5 ), 48 )
        , ( ( 12, 6 ), 49 )
        , ( ( 12, 7 ), 49 )
        , ( ( 12, 8 ), 49 )
        , ( ( 12, 9 ), 49 )
        , ( ( 12, 10 ), 49 )
        , ( ( 12, 11 ), 49 )
        , ( ( 12, 12 ), 49 )
        , ( ( 12, 13 ), 50 )
        , ( ( 12, 14 ), 50 )
        , ( ( 12, 15 ), 50 )
        , ( ( 12, 16 ), 50 )
        , ( ( 12, 17 ), 50 )
        , ( ( 12, 18 ), 50 )
        , ( ( 12, 19 ), 50 )
        , ( ( 12, 20 ), 51 )
        , ( ( 12, 21 ), 51 )
        , ( ( 12, 22 ), 51 )
        , ( ( 12, 23 ), 51 )
        , ( ( 12, 24 ), 51 )
        , ( ( 12, 25 ), 51 )
        , ( ( 12, 26 ), 51 )
        , ( ( 12, 27 ), 52 )
        , ( ( 12, 28 ), 52 )
        , ( ( 12, 29 ), 52 )
        , ( ( 12, 30 ), 52 )
        , ( ( 12, 31 ), 52 )
        ]
