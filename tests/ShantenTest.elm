module ShantenTest exposing (suite)

import Data.Shanten as Shanten
import Data.Tile as Tile
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Shanten module"
        [ describe "Shanten.shantenQidui"
            [ test "6 pairs" <|
                \_ -> Shanten.shantenQidui (Tile.tilesFromString "225588m11p88s223z") |> Expect.equal 0

            -- , test "2 equal pairs" <|
            --     \_ -> Shanten.shantenQidui (Tile.tilesFromString "222288m11p88s223z") |> Expect.equal 1
            ]
        ]
