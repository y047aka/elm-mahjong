module Data.Tile exposing
    ( Tile, Category(..), Value(..)
    , isTerminal, isYaojiu
    , isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
    , toString, fromString
    )

{-|

@docs Tile, Category, Value
@docs isMan, isPin, isSou, isHonor
@docs isTerminal, isYaojiu
@docs isTriplet, isRun, isGang, isPair, isPenchan, isKanchan
@docs toString, fromString

-}


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


{-| 萬子(Manzu):

    Tile Man One False |> toString --> "m1"
    Tile Man Two False |> toString --> "m2"
    Tile Man Three False |> toString --> "m3"
    Tile Man Four False |> toString --> "m4"
    Tile Man Five False |> toString --> "m5"
    Tile Man Six False |> toString --> "m6"
    Tile Man Seven False |> toString --> "m7"
    Tile Man Eight False |> toString --> "m8"
    Tile Man Nine False |> toString --> "m9"

    筒子(Pinzu):

    Tile Pin One False |> toString --> "p1"

    索子(Souzu):

    Tile Sou One False |> toString --> "s1"

    字牌(Honor):

    Tile Honor East False |> toString --> "z1"
    Tile Honor South False |> toString --> "z2"
    Tile Honor West False |> toString --> "z3"
    Tile Honor North False |> toString --> "z4"
    Tile Honor White False |> toString --> "z5"
    Tile Honor Green False |> toString --> "z6"
    Tile Honor Red False |> toString --> "z7"

-}
toString : Tile -> String
toString tile =
    categoryToString tile.category ++ valueToString tile


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

    fromString "m1" --> Just (Tile Man One False)
    fromString "m2" --> Just (Tile Man Two False)
    fromString "m3" --> Just (Tile Man Three False)
    fromString "m4" --> Just (Tile Man Four False)
    fromString "m5" --> Just (Tile Man Five False)
    fromString "m6" --> Just (Tile Man Six False)
    fromString "m7" --> Just (Tile Man Seven False)
    fromString "m8" --> Just (Tile Man Eight False)
    fromString "m9" --> Just (Tile Man Nine False)

    筒子(Pinzu):

    fromString "p1" --> Just (Tile Pin One False)

    索子(Souzu):

    fromString "s1" --> Just (Tile Sou One False)

    字牌(Honor):

    fromString "z1" --> Just (Tile Honor East False)
    fromString "z2" --> Just (Tile Honor South False)
    fromString "z3" --> Just (Tile Honor West False)
    fromString "z4" --> Just (Tile Honor North False)
    fromString "z5" --> Just (Tile Honor White False)
    fromString "z6" --> Just (Tile Honor Green False)
    fromString "z7" --> Just (Tile Honor Red False)

-}
fromString : String -> Maybe Tile
fromString string =
    let
        ( categoryString, valueString ) =
            case String.toList string of
                c :: v :: [] ->
                    ( String.fromChar c, String.fromChar v )

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


sort : List Tile -> List Tile
sort tiles =
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
