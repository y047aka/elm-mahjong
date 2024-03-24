module Data.Tile exposing
    ( Tile
    , isTerminal, isYaojiu
    )

{-|

@docs Tile
@docs isMan, isPin, isSou, isHonour
@docs isTerminal, isYaojiu

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
    | White
    | Green
    | Red


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


isTerminal : Tile -> Bool
isTerminal tile =
    List.member tile [ Man One, Man Nine, Pin One, Pin Nine, Sou One, Sou Nine ]


isYaojiu : Tile -> Bool
isYaojiu tile =
    isTerminal tile || isHonour tile
