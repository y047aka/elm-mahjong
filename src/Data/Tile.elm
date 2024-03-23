module Data.Tile exposing (Tile)


type Tile
    = Man Number
    | Pin Number
    | Sou Number
    | Wind Wind
    | Dragon Dragon


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


type Wind
    = East
    | South
    | West
    | North


type Dragon
    = Red
    | Green
    | White
