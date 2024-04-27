module GroupTest exposing (suite)

import Data.Category exposing (Category(..))
import Data.Group as Group exposing (FindPartialsOption(..), Group(..))
import Data.Tile as Tile exposing (Tile(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Group module"
        [ describe "findGroups"
            [ test "12334567z" <|
                \_ ->
                    Group.findGroupsInSuit FindPartials Honor (Tile.tilesFromString "12334567z")
                        |> Expect.equal [ [ Pair West West ] ]
            , test "225588m" <|
                \_ ->
                    Group.findGroupsInSuit FindPartials Man (Tile.tilesFromString "225588m")
                        |> Expect.equal [ [ Pair M2 M2, Pair (M5 False) (M5 False), Pair M8 M8 ] ]
            , test "12388s" <|
                \_ ->
                    Group.findGroupsInSuit FindPartials Sou (Tile.tilesFromString "12388s")
                        |> Expect.equal [ [ Run S1 S2 S3, Pair S8 S8 ] ]
            ]
        ]
