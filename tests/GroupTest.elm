module GroupTest exposing (suite)

import Data.Group as Group exposing (FindPartialsOption(..), Group(..))
import Data.Tile as Tile exposing (Tile(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Group module"
        [ describe "findGroups"
            [ test "Kokushi" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "19m19p19s12334567z")
                        |> Expect.equal
                            { perSuit =
                                { man = []
                                , pin = []
                                , sou = []
                                , honor = [ [ Pair West West ] ]
                                }
                            }
            , test "Chiitoitsu" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "225588m11p88s2233z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ Pair M2 M2, Pair (M5 False) (M5 False), Pair M8 M8 ] ]
                                , pin = [ [ Pair P1 P1 ] ]
                                , sou = [ [ Pair S8 S8 ] ]
                                , honor = [ [ Pair South South, Pair West West ] ]
                                }
                            }
            , test "456m567p12388s77z" <|
                \_ ->
                    Group.findGroups FindPartials (Tile.tilesFromString "456m567p12388s77z")
                        |> Expect.equal
                            { perSuit =
                                { man = [ [ Run M4 (M5 False) M6 ] ]
                                , pin = [ [ Run (P5 False) P6 P7 ] ]
                                , sou = [ [ Run S1 S2 S3, Pair S8 S8 ] ]
                                , honor = [ [ Pair Red Red ] ]
                                }
                            }
            ]
        ]
