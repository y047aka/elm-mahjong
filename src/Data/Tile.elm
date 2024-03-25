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
    { suit : Suit, value : Value, red : Bool }


type Suit
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

        Red ->
            5

        Green ->
            6

        White ->
            7


isMan : Tile -> Bool
isMan t =
    t.suit == Man


isPin : Tile -> Bool
isPin t =
    t.suit == Pin


isSou : Tile -> Bool
isSou t =
    t.suit == Sou


isHonor : Tile -> Bool
isHonor t =
    t.suit == Honor


isTerminal : Tile -> Bool
isTerminal t =
    not (isHonor t) && (t.value == One || t.value == Nine)


isYaojiu : Tile -> Bool
isYaojiu t =
    isHonor t || isTerminal t


isRedFive : Tile -> Bool
isRedFive t =
    t.red && t.value == Five


isTriplet : ( Tile, Tile, Tile ) -> Bool
isTriplet ( a, b, c ) =
    (a.suit == b.suit && b.suit == c.suit)
        && (a.value == b.value && b.value == c.value)


isRun : ( Tile, Tile, Tile ) -> Bool
isRun ( a, b, c ) =
    not (isHonor a)
        && (a.suit == b.suit && b.suit == c.suit)
        && (valueToInt a + 1 == valueToInt b && valueToInt b + 1 == valueToInt c)


type alias TilesPerSuit =
    { man : List Tile
    , pin : List Tile
    , sou : List Tile
    , honor : List Tile
    }


partitionBySuit : List Tile -> TilesPerSuit
partitionBySuit tiles =
    List.foldr
        (\t acc ->
            case t.suit of
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
