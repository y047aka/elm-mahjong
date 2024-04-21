module Data.Tile exposing
    ( Tile(..), Category(..), Value(..), Honor(..)
    , isTerminal, isYaojiu
    , isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
    , sort, countTiles
    , toString, fromString, tilesToString, tilesFromString
    , compose, partitionByCategory, tileToInt, valueFromInt
    )

{-|

@docs Tile, Category, Value, Honor
@docs isMan, isPin, isSou, isHonor
@docs isTerminal, isYaojiu
@docs isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
@docs sort, countTiles
@docs toString, fromString, tilesToString, tilesFromString

-}

import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..))


type Tile
    = Man Value Bool
    | Pin Value Bool
    | Sou Value Bool
    | Honor Honor


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


type Honor
    = East
    | South
    | West
    | North
    | White
    | Green
    | Red


compose : { category : Category, value : Maybe Value, honor : Maybe Honor } -> Tile
compose tile =
    case ( tile.category, tile.value, tile.honor ) of
        ( Man_, Just v, _ ) ->
            Man v False

        ( Pin_, Just v, _ ) ->
            Pin v False

        ( Sou_, Just v, _ ) ->
            Sou v False

        ( Honor_, _, Just h ) ->
            Honor h

        _ ->
            -- Must never happen
            Man One False


tileToInt : Tile -> Int
tileToInt tile =
    case tile of
        Man value _ ->
            valueToInt value

        Pin value _ ->
            valueToInt value

        Sou value _ ->
            valueToInt value

        Honor East ->
            1

        Honor South ->
            2

        Honor West ->
            3

        Honor North ->
            4

        Honor White ->
            5

        Honor Green ->
            6

        Honor Red ->
            7


valueToInt : Value -> Int
valueToInt v =
    case v of
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


valueFromInt : Int -> Maybe Value
valueFromInt value =
    case value of
        1 ->
            Just One

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        9 ->
            Just Nine

        _ ->
            Nothing


isMan : Tile -> Bool
isMan t =
    case t of
        Man _ _ ->
            True

        _ ->
            False


isPin : Tile -> Bool
isPin t =
    case t of
        Pin _ _ ->
            True

        _ ->
            False


isSou : Tile -> Bool
isSou t =
    case t of
        Sou _ _ ->
            True

        _ ->
            False


isHonor : Tile -> Bool
isHonor t =
    case t of
        Honor _ ->
            True

        _ ->
            False


isSuit : Tile -> Bool
isSuit t =
    not (isHonor t)


isTerminal : Tile -> Bool
isTerminal t =
    isSuit t && (tileToInt t == 1 || tileToInt t == 9)


isYaojiu : Tile -> Bool
isYaojiu t =
    isHonor t || isTerminal t


isRedFive : Tile -> Bool
isRedFive t =
    case t of
        Man value red ->
            red && value == Five

        Pin value red ->
            red && value == Five

        Sou value red ->
            red && value == Five

        _ ->
            False


isSameCategory2 : Tile -> Tile -> Bool
isSameCategory2 a b =
    case ( a, b ) of
        ( Man _ _, Man _ _ ) ->
            True

        ( Pin _ _, Pin _ _ ) ->
            True

        ( Sou _ _, Sou _ _ ) ->
            True

        ( Honor _, Honor _ ) ->
            True

        _ ->
            False


isSameCategory3 : Tile -> Tile -> Tile -> Bool
isSameCategory3 a b c =
    case ( a, b, c ) of
        ( Man _ _, Man _ _, Man _ _ ) ->
            True

        ( Pin _ _, Pin _ _, Pin _ _ ) ->
            True

        ( Sou _ _, Sou _ _, Sou _ _ ) ->
            True

        ( Honor _, Honor _, Honor _ ) ->
            True

        _ ->
            False


isSameCategory4 : Tile -> Tile -> Tile -> Tile -> Bool
isSameCategory4 a b c d =
    case [ a, b, c, d ] of
        [ Man _ _, Man _ _, Man _ _, Man _ _ ] ->
            True

        [ Pin _ _, Pin _ _, Pin _ _, Pin _ _ ] ->
            True

        [ Sou _ _, Sou _ _, Sou _ _, Sou _ _ ] ->
            True

        [ Honor _, Honor _, Honor _, Honor _ ] ->
            True

        _ ->
            False


isTriplet : ( Tile, Tile, Tile ) -> Bool
isTriplet ( a, b, c ) =
    isSameCategory3 a b c
        && (tileToInt a == tileToInt b && tileToInt b == tileToInt c)


isRun : ( Tile, Tile, Tile ) -> Bool
isRun ( a, b, c ) =
    (isSuit a && isSameCategory3 a b c)
        && (tileToInt a + 1 == tileToInt b && tileToInt b + 1 == tileToInt c)


isGang : Tile -> Tile -> Tile -> Tile -> Bool
isGang a b c d =
    isSameCategory4 a b c d
        && (tileToInt a == tileToInt b && tileToInt b == tileToInt c && tileToInt c == tileToInt d)


isPair : ( Tile, Tile ) -> Bool
isPair ( a, b ) =
    isSameCategory2 a b && (tileToInt a == tileToInt b)


isPenchan : ( Tile, Tile ) -> Bool
isPenchan ( a, b ) =
    (isSuit a && isSameCategory2 a b)
        && (tileToInt a + 1 == tileToInt b)


isKanchan : ( Tile, Tile ) -> Bool
isKanchan ( a, b ) =
    (isSuit a && isSameCategory2 a b)
        && (tileToInt a + 2 == tileToInt b)


{-|

    sort [ Man Three False, Man Two False, Man One False ]
    --> [ Man One False, Man Two False, Man Three False ]

    sort [ Sou One False, Honor East, Pin Two False, Man Three False, Man One False ]
    --> [ Man One False, Man Three False, Pin Two False, Sou One False, Honor East ]

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
            case t of
                Man value _ ->
                    valueToInt value

                Pin value _ ->
                    valueToInt value

                Sou value _ ->
                    valueToInt value

                Honor East ->
                    10

                Honor South ->
                    11

                Honor West ->
                    12

                Honor North ->
                    13

                Honor White ->
                    14

                Honor Green ->
                    15

                Honor Red ->
                    16
    in
    List.sortBy toComparable tiles


countTiles : List Tile -> List ( Tile, Int )
countTiles tiles =
    List.Extra.gatherEquals tiles
        |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))


{-| 萬子(Manzu):

    Man One False |> toString --> "1m"
    Man Two False |> toString --> "2m"
    Man Three False |> toString --> "3m"
    Man Four False |> toString --> "4m"
    Man Five False |> toString --> "5m"
    Man Six False |> toString --> "6m"
    Man Seven False |> toString --> "7m"
    Man Eight False |> toString --> "8m"
    Man Nine False |> toString --> "9m"

    筒子(Pinzu):

    Pin One False |> toString --> "1p"

    索子(Souzu):

    Sou One False |> toString --> "1s"

    字牌(Honor):

    Honor East |> toString --> "1z"
    Honor South |> toString --> "2z"
    Honor West |> toString --> "3z"
    Honor North |> toString --> "4z"
    Honor White |> toString --> "5z"
    Honor Green |> toString --> "6z"
    Honor Red |> toString --> "7z"

-}
toString : Tile -> String
toString tile =
    valueToString tile ++ categoryToString (categoryFromTile tile)


type Category
    = Man_
    | Pin_
    | Sou_
    | Honor_


categoryToString : Category -> String
categoryToString category =
    case category of
        Man_ ->
            "m"

        Pin_ ->
            "p"

        Sou_ ->
            "s"

        Honor_ ->
            "z"


categoryFromTile : Tile -> Category
categoryFromTile tile =
    case tile of
        Man _ _ ->
            Man_

        Pin _ _ ->
            Pin_

        Sou _ _ ->
            Sou_

        Honor _ ->
            Honor_


valueToString : Tile -> String
valueToString tile =
    tileToInt tile |> String.fromInt


{-| 萬子(Manzu):

    fromString "1m" --> Just (Man One False)
    fromString "2m" --> Just (Man Two False)
    fromString "3m" --> Just (Man Three False)
    fromString "4m" --> Just (Man Four False)
    fromString "5m" --> Just (Man Five False)
    fromString "6m" --> Just (Man Six False)
    fromString "7m" --> Just (Man Seven False)
    fromString "8m" --> Just (Man Eight False)
    fromString "9m" --> Just (Man Nine False)

    筒子(Pinzu):

    fromString "1p" --> Just (Pin One False)

    索子(Souzu):

    fromString "1s" --> Just (Sou One False)

    字牌(Honor):

    fromString "1z" --> Just (Honor East)
    fromString "2z" --> Just (Honor South)
    fromString "3z" --> Just (Honor West)
    fromString "4z" --> Just (Honor North)
    fromString "5z" --> Just (Honor White)
    fromString "6z" --> Just (Honor Green)
    fromString "7z" --> Just (Honor Red)

-}
fromString : String -> Maybe Tile
fromString string =
    List.head (tilesFromString string)


{-|

    tilesToString [ Man One False, Man One False, Man One False ]
    --> "111m"

    tilesToString [ Man One False, Man Two False, Man Three False ]
    --> "123m"

    tilesToString [ Sou One False, Honor East, Pin Two False, Man Three False, Man One False ]
    --> "13m2p1s1z"

    tilesToString
        [ Man One False, Man Nine False
        , Pin One False, Pin Nine False
        , Sou One False, Sou Nine False
        , Honor East, Honor South, Honor West, Honor West, Honor North
        , Honor White, Honor Green, Honor Red
        ]
    --> "19m19p19s12334567z"

-}
tilesToString : List Tile -> String
tilesToString tiles =
    tiles
        |> sort
        |> List.Extra.gatherEqualsBy categoryFromTile
        |> List.map (\( head, tails ) -> String.concat (List.map valueToString (head :: tails)) ++ categoryToString (categoryFromTile head))
        |> String.concat


{-|

    tilesFromString "111m"
    --> [ Man One False, Man One False, Man One False ]

    tilesFromString "123m"
    --> [ Man One False, Man Two False, Man Three False ]

    tilesFromString "13m2p1s1z"
    --> [ Man One False, Man Three False, Pin Two False, Sou One False, Honor East ]

    tilesFromString "19m19p19s12334567z"
    --> [ Man One False, Man Nine False, Pin One False, Pin Nine False, Sou One False, Sou Nine False, Honor East, Honor South, Honor West, Honor West, Honor North, Honor White, Honor Green, Honor Red ]

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
                    Just Man_

                "p" ->
                    Just Pin_

                "s" ->
                    Just Sou_

                "z" ->
                    Just Honor_

                _ ->
                    Nothing

        maybeHonorTile str =
            case str of
                "1" ->
                    Just (Honor East)

                "2" ->
                    Just (Honor South)

                "3" ->
                    Just (Honor West)

                "4" ->
                    Just (Honor North)

                "5" ->
                    Just (Honor White)

                "6" ->
                    Just (Honor Green)

                "7" ->
                    Just (Honor Red)

                _ ->
                    Nothing

        maybeSuitTile str =
            case str of
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

        values =
            String.dropRight 1 parsedSuit
                |> String.toList
                |> List.map String.fromChar
                |> List.filterMap maybeSuitTile
    in
    case maybeCategory of
        Just Man_ ->
            List.map (\v -> Man v False) values

        Just Pin_ ->
            List.map (\v -> Pin v False) values

        Just Sou_ ->
            List.map (\v -> Sou v False) values

        Just Honor_ ->
            String.dropRight 1 parsedSuit
                |> String.toList
                |> List.map String.fromChar
                |> List.filterMap maybeHonorTile

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
            case t of
                Man _ _ ->
                    { acc | man = t :: acc.man }

                Pin _ _ ->
                    { acc | pin = t :: acc.pin }

                Sou _ _ ->
                    { acc | sou = t :: acc.sou }

                Honor _ ->
                    { acc | honor = t :: acc.honor }
        )
        { sou = [], man = [], pin = [], honor = [] }
        tiles
