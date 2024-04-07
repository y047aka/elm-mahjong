module Data.Counter exposing (Counter, fromIntList, getCount)

import Array


type alias Counter =
    Array.Array Int


fromIntList : List Int -> Counter
fromIntList intList =
    let
        counter =
            Array.initialize 9 (always 0)

        accum : Int -> Array.Array Int -> Array.Array Int
        accum n cnt =
            Array.set (n - 1) (Maybe.withDefault 0 (Array.get (n - 1) cnt) + 1) cnt
    in
    List.foldl accum counter intList


getCount : Int -> Counter -> Int
getCount n counter =
    Array.get n counter
        |> Maybe.withDefault 0
