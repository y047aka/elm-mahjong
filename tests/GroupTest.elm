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
                                { man = [ [ PartialKanchan M4 M6, Pair (M5 False) (M5 False) ], [ Run M4 (M5 False) M6 ] ]
                                , pin = [ [ Pair P3 P3 ] ]
                                , sou = [ [ PartialKanchan S2 S4, Run S2 S3 S4, PartialKanchan (S5 False) S7 ], [ Run S2 S3 S4, PartialKanchan S2 S4, PartialKanchan (S5 False) S7 ], [ PartialPenchan S2 S3, Pair S4 S4, PartialKanchan (S5 False) S7 ], [ Pair S2 S2, PartialPenchan S3 S4, PartialKanchan (S5 False) S7 ], [ Pair S2 S2, PartialPenchan S3 S4, PartialPenchan S4 (S5 False) ] ]
                                , honor = []
                                }
                            }
            , test "5. 7778m23445p456s16z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "7778m23445p456s16z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialPenchan M7 M8, Pair M7 M7 ], [ Pair M7 M7, PartialPenchan M7 M8 ], [ Triplet M7 M7 M7 ] ]
                                , pin = [ [ PartialKanchan P2 P4, Run P3 P4 (P5 False) ], [ Run P2 P3 P4, PartialPenchan P4 (P5 False) ], [ PartialKanchan P3 (P5 False), Pair P4 P4 ], [ PartialPenchan P2 P3, Pair P4 P4 ] ]
                                , sou = [ [ Run S4 (S5 False) S6 ], [ PartialPenchan (S5 False) S6 ], [ PartialKanchan S4 S6 ], [ PartialPenchan S4 (S5 False) ] ]
                                , honor = []
                                }
                            }
            , test "28. 147m234666p56678s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "147m234666p56678s")
                        |> Expect.equal
                            { perSuit =
                                { man = []
                                , pin = [ [ Run P2 P3 P4, Triplet P6 P6 P6 ], [ PartialPenchan P2 P3, PartialKanchan P4 P6, Pair P6 P6 ], [ PartialPenchan P3 P4, Triplet P6 P6 P6 ], [ PartialKanchan P2 P4, Triplet P6 P6 P6 ], [ PartialPenchan P2 P3, Triplet P6 P6 P6 ] ]
                                , sou = [ [ PartialPenchan (S5 False) S6, Run S6 S7 S8 ], [ Run (S5 False) S6 S7, PartialKanchan S6 S8 ], [ Pair S6 S6, PartialPenchan S7 S8 ], [ PartialKanchan (S5 False) S7, Pair S6 S6 ] ]
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
                        |> Expect.equal
                            [ [ Run S1 S2 S3, Pair S8 S8 ]
                            , [ PartialPenchan S2 S3, Pair S8 S8 ]
                            , [ PartialKanchan S1 S3, Pair S8 S8 ]
                            , [ PartialPenchan S1 S2, Pair S8 S8 ]
                            ]
            ]
        ]
