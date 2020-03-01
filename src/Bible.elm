module Bible exposing
    ( Book(..)
    , Passage
    , Ref
    , Testament(..)
    , bookDecoder
    , bookToString
    , compareBooks
    , comparePassage
    , getBookSortPosition
    , newOrOldTestament
    , passageDecoder
    , passageToString
    , refDecoder
    , refToString
    )

import Json.Decode exposing (Decoder)


type Book
    = Genesis -- OT
    | Exodus
    | Leviticus
    | Numbers
    | Deuteronomy
    | Joshua
    | Judges
    | Ruth
    | OneSamuel
    | TwoSamuel
    | OneKings
    | TwoKings
    | OneChronicles
    | TwoChronicles
    | Ezra
    | Nehemiah
    | Esther
    | Job
    | Psalm
    | Proverbs
    | Ecclesiastes
    | SongOfSolomon
    | Isaiah
    | Jeremiah
    | Lamentations
    | Ezekiel
    | Daniel
    | Hosea
    | Joel
    | Amos
    | Obadiah
    | Jonah
    | Micah
    | Nahum
    | Habakkuk
    | Zephaniah
    | Haggai
    | Zechariah
    | Malachi
      -- NT
    | Matthew
    | Mark
    | Luke
    | John
    | Acts
    | Romans
    | OneCorinthians
    | TwoCorinthians
    | Galatians
    | Ephesians
    | Philippians
    | Colossians
    | OneThessalonians
    | TwoThessalonians
    | OneTimothy
    | TwoTimothy
    | Titus
    | Philemon
    | Hebrews
    | James
    | OnePeter
    | TwoPeter
    | OneJohn
    | TwoJohn
    | ThreeJohn
    | Jude
    | Revelation


type Testament
    = NT
    | OT


type alias Ref =
    { book : Book
    , chapter : Int
    }


type alias Passage =
    { start : Ref
    , end : Ref
    }


bookToString : Book -> String
bookToString book =
    case book of
        Genesis ->
            "Genesis"

        Exodus ->
            "Exodus"

        Leviticus ->
            "Leviticus"

        Numbers ->
            "Numbers"

        Deuteronomy ->
            "Deuteronomy"

        Joshua ->
            "Joshua"

        Judges ->
            "Judges"

        Ruth ->
            "Ruth"

        OneSamuel ->
            "1 Samuel"

        TwoSamuel ->
            "2 Samuel"

        OneKings ->
            "1 Kings"

        TwoKings ->
            "2 Kings"

        OneChronicles ->
            "1 Chronicles"

        TwoChronicles ->
            "2 Chronicles"

        Ezra ->
            "Ezra"

        Nehemiah ->
            "Nehemiah"

        Esther ->
            "Esther"

        Job ->
            "Job"

        Psalm ->
            "Psalm"

        Proverbs ->
            "Proverbs"

        Ecclesiastes ->
            "Ecclesiastes"

        SongOfSolomon ->
            "Song of Solomon"

        Isaiah ->
            "Isaiah"

        Jeremiah ->
            "Jeremiah"

        Lamentations ->
            "Lamentations"

        Ezekiel ->
            "Ezekiel"

        Daniel ->
            "Daniel"

        Hosea ->
            "Hosea"

        Joel ->
            "Joel"

        Amos ->
            "Amos"

        Obadiah ->
            "Obadiah"

        Jonah ->
            "Jonah"

        Micah ->
            "Micah"

        Nahum ->
            "Nahum"

        Habakkuk ->
            "Habakkuk"

        Zephaniah ->
            "Zephaniah"

        Haggai ->
            "Haggai"

        Zechariah ->
            "Zechariah"

        Malachi ->
            "Malachi"

        Matthew ->
            "Matthew"

        Mark ->
            "Mark"

        Luke ->
            "Luke"

        John ->
            "John"

        Acts ->
            "Acts"

        Romans ->
            "Romans"

        OneCorinthians ->
            "1 Corinthians"

        TwoCorinthians ->
            "2 Corinthians"

        Galatians ->
            "Galatians"

        Ephesians ->
            "Ephesians"

        Philippians ->
            "Philippians"

        Colossians ->
            "Colossians"

        OneThessalonians ->
            "1 Thessalonians"

        TwoThessalonians ->
            "2 Thessalonians"

        OneTimothy ->
            "1 Timothy"

        TwoTimothy ->
            "2 Timothy"

        Titus ->
            "Titus"

        Philemon ->
            "Philemon"

        Hebrews ->
            "Hebrews"

        James ->
            "James"

        OnePeter ->
            "1 Peter"

        TwoPeter ->
            "2 Peter"

        OneJohn ->
            "1 John"

        TwoJohn ->
            "2 John"

        ThreeJohn ->
            "3 John"

        Jude ->
            "Jude"

        Revelation ->
            "Revelation"


getBookSortPosition : Book -> Int
getBookSortPosition book =
    case book of
        Genesis ->
            1

        Exodus ->
            2

        Leviticus ->
            3

        Numbers ->
            4

        Deuteronomy ->
            5

        Joshua ->
            6

        Judges ->
            7

        Ruth ->
            8

        OneSamuel ->
            9

        TwoSamuel ->
            10

        OneKings ->
            11

        TwoKings ->
            12

        OneChronicles ->
            13

        TwoChronicles ->
            14

        Ezra ->
            15

        Nehemiah ->
            16

        Esther ->
            17

        Job ->
            18

        Psalm ->
            19

        Proverbs ->
            20

        Ecclesiastes ->
            21

        SongOfSolomon ->
            22

        Isaiah ->
            23

        Jeremiah ->
            24

        Lamentations ->
            25

        Ezekiel ->
            26

        Daniel ->
            27

        Hosea ->
            28

        Joel ->
            29

        Amos ->
            30

        Obadiah ->
            31

        Jonah ->
            32

        Micah ->
            33

        Nahum ->
            34

        Habakkuk ->
            35

        Zephaniah ->
            36

        Haggai ->
            37

        Zechariah ->
            38

        Malachi ->
            39

        Matthew ->
            40

        Mark ->
            41

        Luke ->
            42

        John ->
            43

        Acts ->
            44

        Romans ->
            45

        OneCorinthians ->
            46

        TwoCorinthians ->
            47

        Galatians ->
            48

        Ephesians ->
            49

        Philippians ->
            50

        Colossians ->
            51

        OneThessalonians ->
            52

        TwoThessalonians ->
            53

        OneTimothy ->
            54

        TwoTimothy ->
            55

        Titus ->
            56

        Philemon ->
            57

        Hebrews ->
            58

        James ->
            59

        OnePeter ->
            60

        TwoPeter ->
            61

        OneJohn ->
            62

        TwoJohn ->
            63

        ThreeJohn ->
            64

        Jude ->
            65

        Revelation ->
            66


newOrOldTestament : Book -> Testament
newOrOldTestament book =
    if getBookSortPosition book <= 39 then
        OT

    else
        NT


compareBooks : Book -> Book -> Order
compareBooks a b =
    if getBookSortPosition a < getBookSortPosition b then
        GT

    else if getBookSortPosition a > getBookSortPosition b then
        LT

    else
        EQ


comparePassage : Passage -> Passage -> Order
comparePassage a b =
    compareBooks a.start.book b.start.book


refToString : Ref -> String
refToString ref =
    bookToString ref.book ++ String.fromInt ref.chapter


passageToString : Passage -> String
passageToString passage =
    -- Only 1 book
    if passage.start.book == passage.end.book then
        if passage.start.chapter == passage.end.chapter then
            -- Only 1 Chapter
            bookToString passage.start.book ++ " " ++ String.fromInt passage.start.chapter

        else
            -- One book, 2+ chapters
            bookToString passage.start.book
                ++ " "
                ++ String.fromInt passage.start.chapter
                ++ "–"
                ++ String.fromInt passage.end.chapter
        -- Different books

    else
        bookToString passage.start.book
            ++ " "
            ++ String.fromInt passage.start.chapter
            ++ "–"
            ++ bookToString passage.end.book
            ++ " "
            ++ String.fromInt passage.end.chapter


bookDecoder : Decoder Book
bookDecoder =
    Json.Decode.string
        |> Json.Decode.andThen bookStringDecoder


bookStringDecoder : String -> Decoder Book
bookStringDecoder bookString =
    case bookString of
        "Genesis" ->
            Json.Decode.succeed Genesis

        "Gen" ->
            Json.Decode.succeed Genesis

        "Exodus" ->
            Json.Decode.succeed Exodus

        "Leviticus" ->
            Json.Decode.succeed Leviticus

        "Lev" ->
            Json.Decode.succeed Leviticus

        "Numbers" ->
            Json.Decode.succeed Numbers

        "Num" ->
            Json.Decode.succeed Numbers

        "Deuteronomy" ->
            Json.Decode.succeed Deuteronomy

        "Deut" ->
            Json.Decode.succeed Deuteronomy

        "Joshua" ->
            Json.Decode.succeed Joshua

        "Judges" ->
            Json.Decode.succeed Judges

        "Judg" ->
            Json.Decode.succeed Judges

        "Ruth" ->
            Json.Decode.succeed Ruth

        "1 Samuel" ->
            Json.Decode.succeed OneSamuel

        "2 Samuel" ->
            Json.Decode.succeed TwoSamuel

        "1 Sam" ->
            Json.Decode.succeed OneSamuel

        "2 Sam" ->
            Json.Decode.succeed TwoSamuel

        "1 Kings" ->
            Json.Decode.succeed OneKings

        "2 Kings" ->
            Json.Decode.succeed TwoKings

        "1 Chronicles" ->
            Json.Decode.succeed OneChronicles

        "2 Chronicles" ->
            Json.Decode.succeed TwoChronicles

        "1 Chron" ->
            Json.Decode.succeed OneChronicles

        "2 Chron" ->
            Json.Decode.succeed TwoChronicles

        "1 Chr" ->
            Json.Decode.succeed OneChronicles

        "2 Chr" ->
            Json.Decode.succeed TwoChronicles

        "Ezra" ->
            Json.Decode.succeed Ezra

        "Nehemiah" ->
            Json.Decode.succeed Nehemiah

        "Neh" ->
            Json.Decode.succeed Nehemiah

        "Esther" ->
            Json.Decode.succeed Esther

        "Job" ->
            Json.Decode.succeed Job

        "Psalm" ->
            Json.Decode.succeed Psalm

        "Psalms" ->
            Json.Decode.succeed Psalm

        "Ps" ->
            Json.Decode.succeed Psalm

        "Proverbs" ->
            Json.Decode.succeed Proverbs

        "Prov" ->
            Json.Decode.succeed Proverbs

        "Ecclesiastes" ->
            Json.Decode.succeed Ecclesiastes

        "Eccl" ->
            Json.Decode.succeed Ecclesiastes

        "Song of Solomon" ->
            Json.Decode.succeed SongOfSolomon

        "Song of Sol" ->
            Json.Decode.succeed SongOfSolomon

        "Song of Songs" ->
            Json.Decode.succeed SongOfSolomon

        "Isaiah" ->
            Json.Decode.succeed Isaiah

        "Isa" ->
            Json.Decode.succeed Isaiah

        "Is" ->
            Json.Decode.succeed Isaiah

        "Jeremiah" ->
            Json.Decode.succeed Jeremiah

        "Jer" ->
            Json.Decode.succeed Jeremiah

        "Lamentations" ->
            Json.Decode.succeed Lamentations

        "Lam" ->
            Json.Decode.succeed Lamentations

        "Ezekiel" ->
            Json.Decode.succeed Ezekiel

        "Ezek" ->
            Json.Decode.succeed Ezekiel

        "Daniel" ->
            Json.Decode.succeed Daniel

        "Dan" ->
            Json.Decode.succeed Daniel

        "Hosea" ->
            Json.Decode.succeed Hosea

        "Joel" ->
            Json.Decode.succeed Joel

        "Amos" ->
            Json.Decode.succeed Amos

        "Obadiah" ->
            Json.Decode.succeed Obadiah

        "Jonah" ->
            Json.Decode.succeed Jonah

        "Micah" ->
            Json.Decode.succeed Micah

        "Nahum" ->
            Json.Decode.succeed Nahum

        "Habakkuk" ->
            Json.Decode.succeed Habakkuk

        "Zephaniah" ->
            Json.Decode.succeed Zephaniah

        "Haggai" ->
            Json.Decode.succeed Haggai

        "Zechariah" ->
            Json.Decode.succeed Zechariah

        "Zech" ->
            Json.Decode.succeed Zechariah

        "Malachi" ->
            Json.Decode.succeed Malachi

        "Matthew" ->
            Json.Decode.succeed Matthew

        "Matt" ->
            Json.Decode.succeed Matthew

        "Mark" ->
            Json.Decode.succeed Mark

        "Luke" ->
            Json.Decode.succeed Luke

        "John" ->
            Json.Decode.succeed John

        "Acts" ->
            Json.Decode.succeed Acts

        "Romans" ->
            Json.Decode.succeed Romans

        "1 Corinthians" ->
            Json.Decode.succeed OneCorinthians

        "1 Cor" ->
            Json.Decode.succeed OneCorinthians

        "2 Corinthians" ->
            Json.Decode.succeed TwoCorinthians

        "2 Cor" ->
            Json.Decode.succeed TwoCorinthians

        "Galatians" ->
            Json.Decode.succeed Galatians

        "Gal" ->
            Json.Decode.succeed Galatians

        "Ephesians" ->
            Json.Decode.succeed Ephesians

        "Eph" ->
            Json.Decode.succeed Ephesians

        "Philippians" ->
            Json.Decode.succeed Philippians

        "Phil" ->
            Json.Decode.succeed Philippians

        "Colossians" ->
            Json.Decode.succeed Colossians

        "Col" ->
            Json.Decode.succeed Colossians

        "1 Thessalonians" ->
            Json.Decode.succeed OneThessalonians

        "1 Thess" ->
            Json.Decode.succeed OneThessalonians

        "2 Thessalonians" ->
            Json.Decode.succeed TwoThessalonians

        "2 Thess" ->
            Json.Decode.succeed TwoThessalonians

        "1 Timothy" ->
            Json.Decode.succeed OneTimothy

        "1 Tim" ->
            Json.Decode.succeed OneTimothy

        "2 Timothy" ->
            Json.Decode.succeed TwoTimothy

        "2 Tim" ->
            Json.Decode.succeed TwoTimothy

        "Titus" ->
            Json.Decode.succeed Titus

        "Philemon" ->
            Json.Decode.succeed Philemon

        "Hebrews" ->
            Json.Decode.succeed Hebrews

        "Heb" ->
            Json.Decode.succeed Hebrews

        "James" ->
            Json.Decode.succeed James

        "1 Peter" ->
            Json.Decode.succeed OnePeter

        "1 Pet" ->
            Json.Decode.succeed OnePeter

        "2 Peter" ->
            Json.Decode.succeed TwoPeter

        "2 Pet" ->
            Json.Decode.succeed TwoPeter

        "1 John" ->
            Json.Decode.succeed OneJohn

        "2 John" ->
            Json.Decode.succeed TwoJohn

        "3 John" ->
            Json.Decode.succeed ThreeJohn

        "Jude" ->
            Json.Decode.succeed Jude

        "Revelation" ->
            Json.Decode.succeed Revelation

        "Rev" ->
            Json.Decode.succeed Revelation

        _ ->
            Json.Decode.fail <| "I don't know the Bible Book I got: " ++ bookString


refDecoder : Decoder Ref
refDecoder =
    Json.Decode.map2 Ref
        (Json.Decode.field "book" bookDecoder)
        (Json.Decode.field "chapter" Json.Decode.int)


passageDecoder : Decoder Passage
passageDecoder =
    Json.Decode.map2 Passage
        (Json.Decode.field "start" refDecoder)
        (Json.Decode.field "end" refDecoder)
