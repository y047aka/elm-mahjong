module Data.Group exposing (Group(..))

import Data.Tile exposing (Tile)


type Group
    = Triplet Tile Tile Tile
    | Run Tile Tile Tile
    | Gang Tile Tile Tile Tile
    | Pair Tile Tile
    | Penchan Tile Tile
    | Kanchan Tile Tile
