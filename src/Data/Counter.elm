module Data.Counter exposing (Counter, fromTileList, getCount)

import Array
import Data.Tile as Tile exposing (Tile(..))


type alias Counter =
    Array.Array Int


{-|

    import Array
    import Data.Tile exposing (Tile(..))

    fromTileList []
    --> (Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ])

    fromTileList [ M1, M9 ]
    --> (Array.fromList [ 1, 0, 0, 0, 0, 0, 0, 0, 1 ])

    fromTileList [ M1, M2, M3, M4, M5 False, M6, M7, M8, M9 ]
    --> (Array.fromList [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ])

    fromTileList [ M1, M1, M1 ]
    --> (Array.fromList [ 3, 0, 0, 0, 0, 0, 0, 0, 0 ])

-}
fromTileList : List Tile -> Counter
fromTileList intList =
    let
        counter =
            Array.initialize 9 (always 0)

        accum : Int -> Array.Array Int -> Array.Array Int
        accum n cnt =
            Array.set (n - 1) (Maybe.withDefault 0 (Array.get (n - 1) cnt) + 1) cnt
    in
    intList
        |> List.map Tile.tileToInt
        |> List.sort
        |> List.foldl accum counter


getCount : Int -> Counter -> Int
getCount n counter =
    Array.get n counter
        |> Maybe.withDefault 0
