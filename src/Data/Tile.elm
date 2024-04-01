module Data.Tile exposing
    ( Tile
    , isTerminal, isYaojiu
    )

{-|

@docs Tile
@docs isMan, isPin, isSou, isHonor
@docs isTerminal, isYaojiu

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
