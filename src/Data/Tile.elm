module Data.Tile exposing
    ( Tile(..)
    , isTerminal, isYaojiu
    , isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
    , sort, countTiles
    , toString, fromString, tilesToString, tilesFromString
    , compose, fromComparable, manFromInt, partitionByCategory, pinFromInt, souFromInt, tileToInt
    )

{-|

@docs Tile
@docs isMan, isPin, isSou, isHonor
@docs isTerminal, isYaojiu
@docs isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
@docs sort, countTiles
@docs toString, fromString, tilesToString, tilesFromString

-}

import Data.Category as Category exposing (Category(..))
import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..))


type Tile
    = M1
    | M2
    | M3
    | M4
    | M5 Bool
    | M6
    | M7
    | M8
    | M9
    | P1
    | P2
    | P3
    | P4
    | P5 Bool
    | P6
    | P7
    | P8
    | P9
    | S1
    | S2
    | S3
    | S4
    | S5 Bool
    | S6
    | S7
    | S8
    | S9
    | East
    | South
    | West
    | North
    | White
    | Green
    | Red


compose : ( Category, Int ) -> Maybe Tile
compose ( category, value ) =
    case category of
        Man ->
            manFromInt value

        Pin ->
            pinFromInt value

        Sou ->
            souFromInt value

        Honor ->
            honorFromInt value


manFromInt : Int -> Maybe Tile
manFromInt int =
    case int of
        1 ->
            Just M1

        2 ->
            Just M2

        3 ->
            Just M3

        4 ->
            Just M4

        5 ->
            Just (M5 False)

        6 ->
            Just M6

        7 ->
            Just M7

        8 ->
            Just M8

        9 ->
            Just M9

        _ ->
            Nothing


pinFromInt : Int -> Maybe Tile
pinFromInt int =
    case int of
        1 ->
            Just P1

        2 ->
            Just P2

        3 ->
            Just P3

        4 ->
            Just P4

        5 ->
            Just (P5 False)

        6 ->
            Just P6

        7 ->
            Just P7

        8 ->
            Just P8

        9 ->
            Just P9

        _ ->
            Nothing


souFromInt : Int -> Maybe Tile
souFromInt int =
    case int of
        1 ->
            Just S1

        2 ->
            Just S2

        3 ->
            Just S3

        4 ->
            Just S4

        5 ->
            Just (S5 False)

        6 ->
            Just S6

        7 ->
            Just S7

        8 ->
            Just S8

        9 ->
            Just S9

        _ ->
            Nothing


honorFromInt : Int -> Maybe Tile
honorFromInt int =
    case int of
        1 ->
            Just East

        2 ->
            Just South

        3 ->
            Just West

        4 ->
            Just North

        5 ->
            Just White

        6 ->
            Just Green

        7 ->
            Just Red

        _ ->
            Nothing


tileToInt : Tile -> Int
tileToInt tile =
    case tile of
        M1 ->
            1

        M2 ->
            2

        M3 ->
            3

        M4 ->
            4

        M5 _ ->
            5

        M6 ->
            6

        M7 ->
            7

        M8 ->
            8

        M9 ->
            9

        P1 ->
            1

        P2 ->
            2

        P3 ->
            3

        P4 ->
            4

        P5 _ ->
            5

        P6 ->
            6

        P7 ->
            7

        P8 ->
            8

        P9 ->
            9

        S1 ->
            1

        S2 ->
            2

        S3 ->
            3

        S4 ->
            4

        S5 _ ->
            5

        S6 ->
            6

        S7 ->
            7

        S8 ->
            8

        S9 ->
            9

        East ->
            1

        South ->
            2

        West ->
            3

        North ->
            4

        White ->
            5

        Green ->
            6

        Red ->
            7


isMan : Tile -> Bool
isMan t =
    List.member t [ M1, M2, M3, M4, M5 True, M5 False, M6, M7, M8, M9 ]


isPin : Tile -> Bool
isPin t =
    List.member t [ P1, P2, P3, P4, P5 True, P5 False, P6, P7, P8, P9 ]


isSou : Tile -> Bool
isSou t =
    List.member t [ S1, S2, S3, S4, S5 True, S5 False, S6, S7, S8, S9 ]


isHonor : Tile -> Bool
isHonor t =
    List.member t [ East, South, West, North, White, Green, Red ]


isSuit : Tile -> Bool
isSuit t =
    not (isHonor t)


isTerminal : Tile -> Bool
isTerminal t =
    List.member t [ M1, M9, P1, P9, S1, S9 ]


isYaojiu : Tile -> Bool
isYaojiu t =
    isHonor t || isTerminal t


isRedFive : Tile -> Bool
isRedFive t =
    List.member t [ M5 True, P5 True, S5 True ]


isSameCategory2 : Tile -> Tile -> Bool
isSameCategory2 a b =
    Category.isSameCategory2 (toCategory a) (toCategory b)


isSameCategory3 : Tile -> Tile -> Tile -> Bool
isSameCategory3 a b c =
    Category.isSameCategory3 (toCategory a) (toCategory b) (toCategory c)


isTriplet : ( Tile, Tile, Tile ) -> Bool
isTriplet ( a, b, c ) =
    a == b && b == c


isRun : ( Tile, Tile, Tile ) -> Bool
isRun ( a, b, c ) =
    (isSuit a && isSameCategory3 a b c)
        && (tileToInt a + 1 == tileToInt b && tileToInt b + 1 == tileToInt c)


isGang : Tile -> Tile -> Tile -> Tile -> Bool
isGang a b c d =
    a == b && b == c && c == d


isPair : ( Tile, Tile ) -> Bool
isPair ( a, b ) =
    a == b


isPenchan : ( Tile, Tile ) -> Bool
isPenchan ( a, b ) =
    (isSuit a && isSameCategory2 a b)
        && (tileToInt a + 1 == tileToInt b)


isKanchan : ( Tile, Tile ) -> Bool
isKanchan ( a, b ) =
    (isSuit a && isSameCategory2 a b)
        && (tileToInt a + 2 == tileToInt b)


{-|

    sort [ M3, M2, M1 ]
    --> [ M1, M2, M3 ]

    sort [ S1, East, P2, M3, M1 ]
    --> [ M1, M3, P2, S1, East ]

-}
sort : List Tile -> List Tile
sort tiles =
    List.sortBy toComparable tiles


toComparable : Tile -> Int
toComparable t =
    case t of
        M1 ->
            0

        M2 ->
            1

        M3 ->
            2

        M4 ->
            3

        M5 _ ->
            4

        M6 ->
            5

        M7 ->
            6

        M8 ->
            7

        M9 ->
            8

        P1 ->
            9

        P2 ->
            10

        P3 ->
            11

        P4 ->
            12

        P5 _ ->
            13

        P6 ->
            14

        P7 ->
            15

        P8 ->
            16

        P9 ->
            17

        S1 ->
            18

        S2 ->
            19

        S3 ->
            20

        S4 ->
            21

        S5 _ ->
            22

        S6 ->
            23

        S7 ->
            24

        S8 ->
            25

        S9 ->
            26

        East ->
            27

        South ->
            28

        West ->
            29

        North ->
            30

        White ->
            31

        Green ->
            32

        Red ->
            33


fromComparable : Int -> Maybe Tile
fromComparable comparable =
    case ( comparable, comparable // 9, remainderBy 9 comparable ) of
        ( _, 0, i ) ->
            manFromInt (i + 1)

        ( _, 1, i ) ->
            pinFromInt (i + 1)

        ( _, 2, i ) ->
            souFromInt (i + 1)

        ( 27, _, _ ) ->
            Just East

        ( 28, _, _ ) ->
            Just South

        ( 29, _, _ ) ->
            Just West

        ( 30, _, _ ) ->
            Just North

        ( 31, _, _ ) ->
            Just White

        ( 32, _, _ ) ->
            Just Green

        ( 33, _, _ ) ->
            Just Red

        _ ->
            Nothing


countTiles : List Tile -> List ( Tile, Int )
countTiles tiles =
    List.Extra.gatherEquals tiles
        |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))


toCategory : Tile -> Category
toCategory tile =
    if isMan tile then
        Man

    else if isPin tile then
        Pin

    else if isSou tile then
        Sou

    else
        Honor


{-| 萬子(Manzu):

    toString M1 --> "1m"
    toString M2 --> "2m"
    toString M3 --> "3m"
    toString M4 --> "4m"
    toString (M5 False) --> "5m"
    toString M6 --> "6m"
    toString M7 --> "7m"
    toString M8 --> "8m"
    toString M9 --> "9m"

    筒子(Pinzu):

    toString P1 --> "1p"

    索子(Souzu):

    toString S1 --> "1s"

    字牌(Honor):

    toString East --> "1z"
    toString South --> "2z"
    toString West --> "3z"
    toString North --> "4z"
    toString White --> "5z"
    toString Green --> "6z"
    toString Red --> "7z"

-}
toString : Tile -> String
toString tile =
    case tile of
        M1 ->
            "1m"

        M2 ->
            "2m"

        M3 ->
            "3m"

        M4 ->
            "4m"

        M5 _ ->
            "5m"

        M6 ->
            "6m"

        M7 ->
            "7m"

        M8 ->
            "8m"

        M9 ->
            "9m"

        P1 ->
            "1p"

        P2 ->
            "2p"

        P3 ->
            "3p"

        P4 ->
            "4p"

        P5 _ ->
            "5p"

        P6 ->
            "6p"

        P7 ->
            "7p"

        P8 ->
            "8p"

        P9 ->
            "9p"

        S1 ->
            "1s"

        S2 ->
            "2s"

        S3 ->
            "3s"

        S4 ->
            "4s"

        S5 _ ->
            "5s"

        S6 ->
            "6s"

        S7 ->
            "7s"

        S8 ->
            "8s"

        S9 ->
            "9s"

        East ->
            "1z"

        South ->
            "2z"

        West ->
            "3z"

        North ->
            "4z"

        White ->
            "5z"

        Green ->
            "6z"

        Red ->
            "7z"


{-| 萬子(Manzu):

    fromString "1m" --> Just M1
    fromString "2m" --> Just M2
    fromString "3m" --> Just M3
    fromString "4m" --> Just M4
    fromString "5m" --> Just (M5 False)
    fromString "6m" --> Just M6
    fromString "7m" --> Just M7
    fromString "8m" --> Just M8
    fromString "9m" --> Just M9

    筒子(Pinzu):

    fromString "1p" --> Just P1

    索子(Souzu):

    fromString "1s" --> Just S1

    字牌(Honor):

    fromString "1z" --> Just East
    fromString "2z" --> Just South
    fromString "3z" --> Just West
    fromString "4z" --> Just North
    fromString "5z" --> Just White
    fromString "6z" --> Just Green
    fromString "7z" --> Just Red

-}
fromString : String -> Maybe Tile
fromString string =
    case string of
        "1m" ->
            Just M1

        "2m" ->
            Just M2

        "3m" ->
            Just M3

        "4m" ->
            Just M4

        "5m" ->
            Just (M5 False)

        "6m" ->
            Just M6

        "7m" ->
            Just M7

        "8m" ->
            Just M8

        "9m" ->
            Just M9

        "1p" ->
            Just P1

        "2p" ->
            Just P2

        "3p" ->
            Just P3

        "4p" ->
            Just P4

        "5p" ->
            Just (P5 False)

        "6p" ->
            Just P6

        "7p" ->
            Just P7

        "8p" ->
            Just P8

        "9p" ->
            Just P9

        "1s" ->
            Just S1

        "2s" ->
            Just S2

        "3s" ->
            Just S3

        "4s" ->
            Just S4

        "5s" ->
            Just (S5 False)

        "6s" ->
            Just S6

        "7s" ->
            Just S7

        "8s" ->
            Just S8

        "9s" ->
            Just S9

        "1z" ->
            Just East

        "2z" ->
            Just South

        "3z" ->
            Just West

        "4z" ->
            Just North

        "5z" ->
            Just White

        "6z" ->
            Just Green

        "7z" ->
            Just Red

        _ ->
            Nothing


{-|

    tilesToString [ M1, M1, M1 ]
    --> "111m"

    tilesToString [ M1, M2, M3 ]
    --> "123m"

    tilesToString [ S1, East, P2, M3, M1 ]
    --> "13m2p1s1z"

    tilesToString
        [ M1, M9
        , P1, P9
        , S1, S9
        , East, South, West, West, North
        , White, Green, Red
        ]
    --> "19m19p19s12334567z"

-}
tilesToString : List Tile -> String
tilesToString tiles =
    let
        toString_ category tiles_ =
            case tiles_ of
                [] ->
                    ""

                ts ->
                    String.concat (List.map (tileToInt >> String.fromInt) (sort ts)) ++ category
    in
    tiles
        |> partitionByCategory
        |> (\{ man, pin, sou, honor } ->
                String.concat
                    [ toString_ "m" man
                    , toString_ "p" pin
                    , toString_ "s" sou
                    , toString_ "z" honor
                    ]
           )


{-|

    tilesFromString "111m"
    --> [ M1, M1, M1 ]

    tilesFromString "123m"
    --> [ M1, M2, M3 ]

    tilesFromString "13m2p1s1z"
    --> [ M1, M3, P2, S1, East ]

    tilesFromString "19m19p19s12334567z"
    --> [ M1, M9, P1, P9, S1, S9, East, South, West, West, North, White, Green, Red ]

-}
tilesFromString : String -> List Tile
tilesFromString input =
    case Parser.run handSuits input of
        Ok value ->
            value

        Err _ ->
            []


handSuits : Parser (List Tile)
handSuits =
    Parser.loop [] parseHandHelper


parseHandHelper : List Tile -> Parser (Step (List Tile) (List Tile))
parseHandHelper parsedSuits =
    Parser.oneOf
        [ Parser.succeed (\hand -> Loop (List.append parsedSuits hand))
            |= handSuit
        , Parser.succeed ()
            |> Parser.map (\_ -> Done parsedSuits)
        ]


handSuit : Parser.Parser (List Tile)
handSuit =
    Parser.map tilesFromSuitString <|
        Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompWhile (\c -> Char.isDigit c)
                |. Parser.chompIf (\c -> c == 's' || c == 'm' || c == 'p' || c == 'z')


tilesFromSuitString : String -> List Tile
tilesFromSuitString parsedSuit =
    let
        categoryString =
            String.right 1 parsedSuit
    in
    String.dropRight 1 parsedSuit
        |> String.toList
        |> List.filterMap (\v -> fromString (String.fromChar v ++ categoryString))


type alias TilesPerCategory =
    { man : List Tile
    , pin : List Tile
    , sou : List Tile
    , honor : List Tile
    }


partitionByCategory : List Tile -> TilesPerCategory
partitionByCategory tiles =
    List.foldr
        (\t acc ->
            case toCategory t of
                Man ->
                    { acc | man = t :: acc.man }

                Pin ->
                    { acc | pin = t :: acc.pin }

                Sou ->
                    { acc | sou = t :: acc.sou }

                Honor ->
                    { acc | honor = t :: acc.honor }
        )
        { sou = [], man = [], pin = [], honor = [] }
        tiles
