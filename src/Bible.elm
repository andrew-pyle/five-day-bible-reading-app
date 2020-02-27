module Bible exposing
    ( Book
    , Passage
    , Ref
    , Testament
    , bookToString
    , compareBooks
    , comparePassage
    , getBookSortPosition
    , newOrOldTestament
    , passageToString
    , refToString
    )


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
