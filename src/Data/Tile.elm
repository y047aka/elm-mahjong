module Data.Tile exposing (Tile)


type Tile
    = Man Number
    | Pin Number
    | Sou Number
    | Honour Honour


type Number
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


type Honour
    = East
    | South
    | West
    | North
    | Red
    | Green
    | White
