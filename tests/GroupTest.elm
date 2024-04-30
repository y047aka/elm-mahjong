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
            [ test "3698. 1234579m446789p3s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "1234579m446789p3s")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialPenchan M1 M2, Run M3 M4 (M5 False), PartialKanchan M7 M9 ], [ Run M1 M2 M3, PartialPenchan M4 (M5 False), PartialKanchan M7 M9 ] ]
                                , pin = [ [ Pair P4 P4, PartialKanchan P6 P8, PartialKanchan P7 P9 ], [ Pair P4 P4, PartialPenchan P6 P7, PartialPenchan P8 P9 ], [ PartialKanchan P4 P6, Run P7 P8 P9 ], [ Pair P4 P4, Run P7 P8 P9 ], [ Pair P4 P4, Run P6 P7 P8 ] ]
                                , sou = []
                                , honor = []
                                }
                            }
            , test "4321. 3555778m2356p35s4z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "3555778m2356p35s4z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialKanchan (M5 False) M7, Pair (M5 False) (M5 False), PartialPenchan M7 M8 ], [ Pair (M5 False) (M5 False), PartialKanchan (M5 False) M7, PartialPenchan M7 M8 ], [ PartialKanchan M3 (M5 False), Pair (M5 False) (M5 False), PartialPenchan M7 M8 ], [ Triplet (M5 False) (M5 False) (M5 False), PartialPenchan M7 M8 ], [ Triplet (M5 False) (M5 False) (M5 False), Pair M7 M7 ] ]
                                , pin = [ [ PartialPenchan P2 P3, PartialPenchan (P5 False) P6 ] ]
                                , sou = [ [ PartialKanchan S3 (S5 False) ] ]
                                , honor = []
                                }
                            }
            , test "5762. 2389m16p13445679s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "2389m16p13445679s")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialPenchan M2 M3, PartialPenchan M8 M9 ] ]
                                , pin = []
                                , sou = [ [ PartialKanchan S1 S3, PartialKanchan S4 S6, PartialPenchan S4 (S5 False), PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, PartialPenchan S4 (S5 False), PartialKanchan S4 S6, PartialKanchan S7 S9 ], [ PartialPenchan S3 S4, Run S4 (S5 False) S6, PartialKanchan S7 S9 ], [ Run S3 S4 (S5 False), PartialKanchan S4 S6, PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, Run S4 (S5 False) S6, PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, Pair S4 S4, PartialPenchan (S5 False) S6, PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, Pair S4 S4, Run (S5 False) S6 S7 ] ]
                                , honor = []
                                }
                            }
            , test "6207. 1679m89p12557789s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "1679m89p12557789s")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialKanchan M7 M9 ], [ PartialPenchan M6 M7 ] ]
                                , pin = [ [ PartialPenchan P8 P9 ] ]
                                , sou = [ [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, PartialKanchan (S5 False) S7, PartialPenchan S8 S9 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, Run S7 S8 S9 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), PartialKanchan S7 S9, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), PartialPenchan S7 S8, PartialKanchan S7 S9 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), Run S7 S8 S9 ] ]
                                , honor = []
                                }
                            }
            , test "6212. 234556m13579p167s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "234556m13579p167s")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialKanchan M2 M4, PartialKanchan M3 (M5 False), PartialPenchan (M5 False) M6 ], [ PartialPenchan M2 M3, PartialPenchan M4 (M5 False), PartialPenchan (M5 False) M6 ], [ PartialKanchan M3 (M5 False), Run M4 (M5 False) M6 ], [ Run M3 M4 (M5 False), PartialPenchan (M5 False) M6 ], [ PartialPenchan M2 M3, Run M4 (M5 False) M6 ], [ Run M2 M3 M4, PartialPenchan (M5 False) M6 ], [ PartialPenchan M2 M3, PartialKanchan M4 M6, Pair (M5 False) (M5 False) ], [ Run M2 M3 M4, Pair (M5 False) (M5 False) ] ]
                                , pin = [ [ PartialKanchan P3 (P5 False), PartialKanchan P7 P9 ], [ PartialKanchan P1 P3, PartialKanchan P7 P9 ], [ PartialKanchan P1 P3, PartialKanchan (P5 False) P7 ] ]
                                , sou = [ [ PartialPenchan S6 S7 ] ]
                                , honor = []
                                }
                            }
            , test "6380. 458m567889p2357s7z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "458m567889p2357s7z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialPenchan M4 (M5 False) ] ]
                                , pin = [ [ PartialKanchan (P5 False) P7, PartialKanchan P6 P8, PartialPenchan P8 P9 ], [ PartialPenchan (P5 False) P6, PartialPenchan P7 P8, PartialPenchan P8 P9 ], [ PartialKanchan P6 P8, Run P7 P8 P9 ], [ Run P6 P7 P8, PartialPenchan P8 P9 ], [ PartialPenchan (P5 False) P6, Run P7 P8 P9 ], [ Run (P5 False) P6 P7, PartialPenchan P8 P9 ], [ PartialPenchan (P5 False) P6, PartialKanchan P7 P9, Pair P8 P8 ], [ Run (P5 False) P6 P7, Pair P8 P8 ] ]
                                , sou = [ [ PartialPenchan S2 S3, PartialKanchan (S5 False) S7 ] ]
                                , honor = []
                                }
                            }
            , test "6896. 6779m12556778s46z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "6779m12556778s46z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialPenchan M6 M7, PartialKanchan M7 M9 ], [ Pair M7 M7 ] ]
                                , pin = []
                                , sou = [ [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, PartialKanchan (S5 False) S7, PartialKanchan S6 S8 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, PartialPenchan (S5 False) S6, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, PartialPenchan (S5 False) S6, PartialKanchan (S5 False) S7, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, Run S6 S7 S8 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, Run (S5 False) S6 S7 ], [ PartialPenchan S1 S2, Run (S5 False) S6 S7, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, Run (S5 False) S6 S7, PartialKanchan (S5 False) S7 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), PartialKanchan S6 S8, Pair S7 S7 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), Run S6 S7 S8 ] ]
                                , honor = []
                                }
                            }
            , test "8190. 479m234556p6889s7z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "479m234556p6889s7z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialKanchan M7 M9 ] ]
                                , pin = [ [ PartialKanchan P2 P4, PartialKanchan P3 (P5 False), PartialPenchan (P5 False) P6 ], [ PartialPenchan P2 P3, PartialPenchan P4 (P5 False), PartialPenchan (P5 False) P6 ], [ PartialKanchan P3 (P5 False), Run P4 (P5 False) P6 ], [ Run P3 P4 (P5 False), PartialPenchan (P5 False) P6 ], [ PartialPenchan P2 P3, Run P4 (P5 False) P6 ], [ Run P2 P3 P4, PartialPenchan (P5 False) P6 ], [ PartialPenchan P2 P3, PartialKanchan P4 P6, Pair (P5 False) (P5 False) ], [ Run P2 P3 P4, Pair (P5 False) (P5 False) ] ]
                                , sou = [ [ PartialKanchan S6 S8, PartialPenchan S8 S9 ], [ Pair S8 S8 ] ]
                                , honor = []
                                }
                            }
            , test "9194. 24m567p122467889s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "24m567p122467889s")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialKanchan M2 M4 ] ]
                                , pin = [ [ Run (P5 False) P6 P7 ] ]
                                , sou = [ [ PartialPenchan S1 S2, PartialKanchan S2 S4, PartialKanchan S6 S8, Run S7 S8 S9 ], [ PartialPenchan S1 S2, PartialKanchan S2 S4, Run S6 S7 S8, PartialPenchan S8 S9 ], [ Pair S2 S2, PartialKanchan S4 S6, PartialKanchan S7 S9, Pair S8 S8 ], [ Pair S2 S2, PartialKanchan S6 S8, Run S7 S8 S9 ], [ Pair S2 S2, Run S6 S7 S8, PartialPenchan S8 S9 ], [ Pair S2 S2, PartialKanchan S4 S6, Run S7 S8 S9 ] ]
                                , honor = []
                                }
                            }
            , test "9593. 25668m224456p157s" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "25668m224456p157s")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ PartialPenchan (M5 False) M6, PartialKanchan M6 M8 ], [ Pair M6 M6 ] ]
                                , pin = [ [ PartialKanchan P2 P4, PartialKanchan P2 P4, PartialPenchan (P5 False) P6 ], [ PartialKanchan P2 P4, Run P4 (P5 False) P6 ], [ Pair P2 P2, PartialKanchan P4 P6, PartialPenchan P4 (P5 False) ], [ Pair P2 P2, PartialPenchan P4 (P5 False), PartialKanchan P4 P6 ], [ Pair P2 P2, Run P4 (P5 False) P6 ] ]
                                , sou = [ [ PartialKanchan (S5 False) S7 ] ]
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
