module Data.Tile exposing
    ( Tile
    , isTerminal, isYaojiu
    )

{-|

@docs Tile
@docs isMan, isPin, isSou, isHonour
@docs isTerminal, isYaojiu

-}


type alias Tile =
    { suit : Suit, value : Value, red : Bool }


type Suit
    = Man
    | Pin
    | Sou
    | Honour


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
      -- Honours
    | East
    | South
    | West
    | North
    | White
    | Green
    | Red


isMan : Tile -> Bool
isMan t =
    t.suit == Man


isPin : Tile -> Bool
isPin t =
    t.suit == Pin


isSou : Tile -> Bool
isSou t =
    t.suit == Sou


isHonour : Tile -> Bool
isHonour t =
    t.suit == Honour


isTerminal : Tile -> Bool
isTerminal t =
    not (isHonour t) && (t.value == One || t.value == Nine)


isYaojiu : Tile -> Bool
isYaojiu t =
    isHonour t || isTerminal t


isRedFive : Tile -> Bool
isRedFive t =
    t.red && t.value == Five


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

                Honour ->
                    { acc | honor = t :: acc.honor }
        )
        { sou = [], man = [], pin = [], honor = [] }
        tiles
