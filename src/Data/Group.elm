module Data.Group exposing (Group(..))

import Data.Tile exposing (Tile)
import List.Extra


type Group
    = Peng Tile Tile Tile
    | Chi Tile Tile Tile
    | Gang Tile Tile Tile Tile


uniqueDuizis : List Tile -> List ( Tile, Tile )
uniqueDuizis tiles =
    tiles
        |> List.Extra.gatherEquals
        |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))
        |> List.filter (\( _, count ) -> count >= 2)
        |> List.map (\( t, _ ) -> ( t, t ))


uniquePengs : List Tile -> List ( Tile, Tile, Tile )
uniquePengs tiles =
    tiles
        |> List.Extra.gatherEquals
        |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))
        |> List.filter (\( _, count ) -> count >= 3)
        |> List.map (\( t, _ ) -> ( t, t, t ))
