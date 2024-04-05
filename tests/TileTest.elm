module TileTest exposing (suite)

import Data.Tile as Tile exposing (Category(..), Tile, Value(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Tile module"
        [ describe "Tile.toString"
            -- Nest as many descriptions as you like.
            [ test "萬子(Manzu)" <|
                \_ ->
                    Tile Man One False
                        |> Tile.toString
                        |> Expect.equal "m1"
            , test "筒子(Pinzu)" <|
                \_ ->
                    Tile Pin One False
                        |> Tile.toString
                        |> Expect.equal "p1"
            , test "索子(Souzu)" <|
                \_ ->
                    Tile Sou One False
                        |> Tile.toString
                        |> Expect.equal "s1"
            , test "風牌(Wind)" <|
                \_ ->
                    Tile Honor East False
                        |> Tile.toString
                        |> Expect.equal "z1"
            , test "三元牌(Dragon)" <|
                \_ ->
                    Tile Honor Red False
                        |> Tile.toString
                        |> Expect.equal "z7"
            ]
        ]
