module Data.Tile exposing
    ( Tile, Category(..), Value(..)
    , isTerminal, isYaojiu
    , isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
    , sort
    , toString, fromString, tilesToString, tilesFromString
    )

{-|

@docs Tile, Category, Value
@docs isMan, isPin, isSou, isHonor
@docs isTerminal, isYaojiu
@docs isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
@docs sort
@docs toString, fromString, tilesToString, tilesFromString

-}

import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..))


type alias Tile =
    { category : Category, value : Value, red : Bool }


type Category
    = Man
    | Pin
    | Sou
    | Honor


type Value
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
      -- Honors
    | East
    | South
    | West
    | North
    | White
    | Green
    | Red


valueToInt : Tile -> Int
valueToInt tile =
    case tile.value of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
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
    t.category == Man


isPin : Tile -> Bool
isPin t =
    t.category == Pin


isSou : Tile -> Bool
isSou t =
    t.category == Sou


isHonor : Tile -> Bool
isHonor t =
    t.category == Honor


isSuit : Tile -> Bool
isSuit t =
    t.category /= Honor


isTerminal : Tile -> Bool
isTerminal t =
    isSuit t && (t.value == One || t.value == Nine)


isYaojiu : Tile -> Bool
isYaojiu t =
    isHonor t || isTerminal t


isRedFive : Tile -> Bool
isRedFive t =
    t.red && t.value == Five


isTriplet : ( Tile, Tile, Tile ) -> Bool
isTriplet ( a, b, c ) =
    (a.category == b.category && b.category == c.category)
        && (a.value == b.value && b.value == c.value)


isRun : ( Tile, Tile, Tile ) -> Bool
isRun ( a, b, c ) =
    (isSuit a && a.category == b.category && b.category == c.category)
        && (valueToInt a + 1 == valueToInt b && valueToInt b + 1 == valueToInt c)


isGang : Tile -> Tile -> Tile -> Tile -> Bool
isGang a b c d =
    (a.category == b.category && b.category == c.category && c.category == d.category)
        && (a.value == b.value && b.value == c.value && c.value == d.value)


isPair : ( Tile, Tile ) -> Bool
isPair ( a, b ) =
    a.category == b.category && a.value == b.value


isPenchan : ( Tile, Tile ) -> Bool
isPenchan ( a, b ) =
    (isSuit a && a.category == b.category)
        && (valueToInt a + 1 == valueToInt b)


isKanchan : ( Tile, Tile ) -> Bool
isKanchan ( a, b ) =
    (isSuit a && a.category == b.category)
        && (valueToInt a + 2 == valueToInt b)


{-|

    sort [ Tile Man Three False, Tile Man Two False, Tile Man One False ]
    --> [ Tile Man One False, Tile Man Two False, Tile Man Three False ]

    sort [ Tile Sou One False, Tile Honor East False, Tile Pin Two False, Tile Man Three False, Tile Man One False ]
    --> [ Tile Man One False, Tile Man Three False, Tile Pin Two False, Tile Sou One False, Tile Honor East False ]

-}
sort : List Tile -> List Tile
sort tiles =
    tiles
        |> partitionByCategory
        |> (\{ man, pin, sou, honor } -> List.concatMap sortByValue [ man, pin, sou, honor ])


sortByValue : List Tile -> List Tile
sortByValue tiles =
    let
        toComparable t =
            case t.value of
                One ->
                    1

                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

                Seven ->
                    7

                Eight ->
                    8

                Nine ->
                    9

                East ->
                    10

                South ->
                    11

                West ->
                    12

                North ->
                    13

                White ->
                    14

                Green ->
                    15

                Red ->
                    16
    in
    List.sortBy toComparable tiles


{-| 萬子(Manzu):

    Tile Man One False |> toString --> "1m"
    Tile Man Two False |> toString --> "2m"
    Tile Man Three False |> toString --> "3m"
    Tile Man Four False |> toString --> "4m"
    Tile Man Five False |> toString --> "5m"
    Tile Man Six False |> toString --> "6m"
    Tile Man Seven False |> toString --> "7m"
    Tile Man Eight False |> toString --> "8m"
    Tile Man Nine False |> toString --> "9m"

    筒子(Pinzu):

    Tile Pin One False |> toString --> "1p"

    索子(Souzu):

    Tile Sou One False |> toString --> "1s"

    字牌(Honor):

    Tile Honor East False |> toString --> "1z"
    Tile Honor South False |> toString --> "2z"
    Tile Honor West False |> toString --> "3z"
    Tile Honor North False |> toString --> "4z"
    Tile Honor White False |> toString --> "5z"
    Tile Honor Green False |> toString --> "6z"
    Tile Honor Red False |> toString --> "7z"

-}
toString : Tile -> String
toString tile =
    valueToString tile ++ categoryToString tile.category


categoryToString : Category -> String
categoryToString category =
    case category of
        Man ->
            "m"

        Pin ->
            "p"

        Sou ->
            "s"

        Honor ->
            "z"


valueToString : Tile -> String
valueToString tile =
    valueToInt tile |> String.fromInt


{-| 萬子(Manzu):

    fromString "1m" --> Just (Tile Man One False)
    fromString "2m" --> Just (Tile Man Two False)
    fromString "3m" --> Just (Tile Man Three False)
    fromString "4m" --> Just (Tile Man Four False)
    fromString "5m" --> Just (Tile Man Five False)
    fromString "6m" --> Just (Tile Man Six False)
    fromString "7m" --> Just (Tile Man Seven False)
    fromString "8m" --> Just (Tile Man Eight False)
    fromString "9m" --> Just (Tile Man Nine False)

    筒子(Pinzu):

    fromString "1p" --> Just (Tile Pin One False)

    索子(Souzu):

    fromString "1s" --> Just (Tile Sou One False)

    字牌(Honor):

    fromString "1z" --> Just (Tile Honor East False)
    fromString "2z" --> Just (Tile Honor South False)
    fromString "3z" --> Just (Tile Honor West False)
    fromString "4z" --> Just (Tile Honor North False)
    fromString "5z" --> Just (Tile Honor White False)
    fromString "6z" --> Just (Tile Honor Green False)
    fromString "7z" --> Just (Tile Honor Red False)

-}
fromString : String -> Maybe Tile
fromString string =
    let
        ( valueString, categoryString ) =
            case String.toList string of
                v :: c :: [] ->
                    ( String.fromChar v, String.fromChar c )

                _ ->
                    ( "", "" )

        maybeCategory =
            case categoryString of
                "m" ->
                    Just Man

                "p" ->
                    Just Pin

                "s" ->
                    Just Sou

                "z" ->
                    Just Honor

                _ ->
                    Nothing

        maybeValue =
            maybeCategory
                |> Maybe.andThen
                    (\c ->
                        case c of
                            Honor ->
                                case valueString of
                                    "1" ->
                                        Just East

                                    "2" ->
                                        Just South

                                    "3" ->
                                        Just West

                                    "4" ->
                                        Just North

                                    "5" ->
                                        Just White

                                    "6" ->
                                        Just Green

                                    "7" ->
                                        Just Red

                                    _ ->
                                        Nothing

                            _ ->
                                case valueString of
                                    "1" ->
                                        Just One

                                    "2" ->
                                        Just Two

                                    "3" ->
                                        Just Three

                                    "4" ->
                                        Just Four

                                    "5" ->
                                        Just Five

                                    "6" ->
                                        Just Six

                                    "7" ->
                                        Just Seven

                                    "8" ->
                                        Just Eight

                                    "9" ->
                                        Just Nine

                                    _ ->
                                        Nothing
                    )
    in
    Maybe.map2 (\c v -> { category = c, value = v, red = False }) maybeCategory maybeValue


{-|

    tilesToString [ Tile Man One False, Tile Man One False, Tile Man One False ]
    --> "111m"

    tilesToString [ Tile Man One False, Tile Man Two False, Tile Man Three False ]
    --> "123m"

    tilesToString [ Tile Sou One False, Tile Honor East False, Tile Pin Two False, Tile Man Three False, Tile Man One False ]
    --> "13m2p1s1z"

    tilesToString
        [ Tile Man One False, Tile Man Nine False
        , Tile Pin One False, Tile Pin Nine False
        , Tile Sou One False, Tile Sou Nine False
        , Tile Honor East False, Tile Honor South False, Tile Honor West False, Tile Honor West False, Tile Honor North False
        , Tile Honor White False, Tile Honor Green False, Tile Honor Red False
        ]
    --> "19m19p19s12334567z"

-}
tilesToString : List Tile -> String
tilesToString tiles =
    tiles
        |> sort
        |> List.Extra.gatherEqualsBy .category
        |> List.map (\( head, tails ) -> String.concat (List.map valueToString (head :: tails)) ++ categoryToString head.category)
        |> String.concat


{-|

    tilesFromString "111m"
    --> [ Tile Man One False, Tile Man One False, Tile Man One False ]

    tilesFromString "123m"
    --> [ Tile Man One False, Tile Man Two False, Tile Man Three False ]

    tilesFromString "13m2p1s1z"
    --> [ Tile Man One False, Tile Man Three False, Tile Pin Two False, Tile Sou One False, Tile Honor East False ]

    tilesFromString "19m19p19s12334567z"
    --> [ Tile Man One False, Tile Man Nine False, Tile Pin One False, Tile Pin Nine False, Tile Sou One False, Tile Sou Nine False, Tile Honor East False, Tile Honor South False, Tile Honor West False, Tile Honor West False, Tile Honor North False, Tile Honor White False, Tile Honor Green False, Tile Honor Red False ]

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
        maybeCategory =
            case String.right 1 parsedSuit of
                "m" ->
                    Just Man

                "p" ->
                    Just Pin

                "s" ->
                    Just Sou

                "z" ->
                    Just Honor

                _ ->
                    Nothing

        tiles =
            String.dropRight 1 parsedSuit
                |> String.toList
                |> List.map String.fromChar
                |> List.filterMap maybeValue

        maybeValue valueString =
            maybeCategory
                |> Maybe.andThen
                    (\c ->
                        case c of
                            Honor ->
                                case valueString of
                                    "1" ->
                                        Just East

                                    "2" ->
                                        Just South

                                    "3" ->
                                        Just West

                                    "4" ->
                                        Just North

                                    "5" ->
                                        Just White

                                    "6" ->
                                        Just Green

                                    "7" ->
                                        Just Red

                                    _ ->
                                        Nothing

                            _ ->
                                case valueString of
                                    "1" ->
                                        Just One

                                    "2" ->
                                        Just Two

                                    "3" ->
                                        Just Three

                                    "4" ->
                                        Just Four

                                    "5" ->
                                        Just Five

                                    "6" ->
                                        Just Six

                                    "7" ->
                                        Just Seven

                                    "8" ->
                                        Just Eight

                                    "9" ->
                                        Just Nine

                                    _ ->
                                        Nothing
                    )
    in
    case maybeCategory of
        Just c ->
            List.map (\v -> Tile c v False) tiles

        Nothing ->
            []


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
            case t.category of
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
