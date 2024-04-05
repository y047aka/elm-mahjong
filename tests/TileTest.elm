module TileTest exposing (suite)

import Data.Tile as Tile exposing (Category(..), Tile, Value(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Tile module"
        [ test "萬子(Manzu)" <|
            \_ ->
                Tile Man One False
                    |> Tile.toString
                    |> Expect.equal "1m"
        ]
