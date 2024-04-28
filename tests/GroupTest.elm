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
            [ test "0. 4556m33p2234457s1z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "4556m33p2234457s1z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ Run M4 (M5 False) M6 ] ]
                                , pin = [ [ Pair P3 P3 ] ]
                                , sou = [ [ Pair S2 S2, Run S3 S4 (S5 False) ] ]
                                , honor = []
                                }
                            }
            , test "5. 7778m23445p456s16z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "7778m23445p456s16z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ Triplet M7 M7 M7 ] ]
                                , pin = [ [ Run P2 P3 P4, PartialPenchan P4 (P5 False) ], [ PartialKanchan P2 P4, Run P3 P4 (P5 False) ] ]
                                , sou = [ [ Run S4 (S5 False) S6 ] ]
                                , honor = []
                                }
                            }
            , test "28. 147m234666p56678s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "147m234666p56678s")
                        |> Expect.equal
                            { perSuit =
                                { man = []
                                , pin = [ [ Run P2 P3 P4, Triplet P6 P6 P6 ] ]
                                , sou = [ [ Run (S5 False) S6 S7, PartialKanchan S6 S8 ], [ PartialPenchan (S5 False) S6, Run S6 S7 S8 ] ]
                                , honor = []
                                }
                            }
            ]
        , describe "findGroupsInSuit"
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
