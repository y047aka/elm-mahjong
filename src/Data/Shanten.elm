module Data.Shanten exposing (shantenGuoshi, shantenQidui)

import Data.Tile as Tile exposing (Tile)
import List.Extra


{-|

    import Data.Tile exposing (tilesFromString)

    shantenGuoshi (tilesFromString "19m19p19s1234567z") --> 0
    shantenGuoshi (tilesFromString "12m19p19s1234567z") --> 1
    shantenGuoshi (tilesFromString "19m19p19s12334567z") --> -1

-}
shantenGuoshi : List Tile -> Int
shantenGuoshi tiles =
    let
        counter : List ( Tile, Int )
        counter =
            tiles
                |> List.filter Tile.isYaojiu
                |> List.Extra.gatherEquals
                |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))

        yaojiuCount =
            List.length counter

        hasPair =
            List.any (\( _, count ) -> count >= 2) counter
    in
    if hasPair then
        12 - yaojiuCount

    else
        13 - yaojiuCount


{-|

    import Data.Tile exposing (tilesFromString)

    shantenQidui (tilesFromString "1122334455667m") --> 0
    shantenQidui (tilesFromString "1122334455m12p") --> 1
    shantenQidui (tilesFromString "11223344556677m") --> -1

-}
shantenQidui : List Tile -> Int
shantenQidui tiles =
    let
        counter : List ( Tile, Int )
        counter =
            tiles
                |> List.Extra.gatherEquals
                |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))

        duiziCount =
            List.length <| List.filter (\( _, count ) -> count >= 2) counter

        guliCount =
            List.length <| List.filter (\( _, count ) -> count == 1) counter

        clampedDuiziCount =
            min duiziCount 7

        clampedGuliCount =
            min guliCount (7 - clampedDuiziCount)
    in
    13 - (clampedDuiziCount * 2) - clampedGuliCount
