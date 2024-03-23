module Data.Shanten exposing (..)

import Data.Tile as Tile exposing (Tile)
import List.Extra


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
