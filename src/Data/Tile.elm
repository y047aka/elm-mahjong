module Data.Tile exposing
    ( Tile
    , isMan, isPin, isSou, isHonour
    )

{-|

@docs Tile
@docs isMan, isPin, isSou, isHonour

-}


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


isMan : Tile -> Bool
isMan tile =
    case tile of
        Man _ ->
            True

        _ ->
            False


isPin : Tile -> Bool
isPin tile =
    case tile of
        Pin _ ->
            True

        _ ->
            False


isSou : Tile -> Bool
isSou tile =
    case tile of
        Sou _ ->
            True

        _ ->
            False


isHonour : Tile -> Bool
isHonour tile =
    case tile of
        Honour _ ->
            True

        _ ->
            False
