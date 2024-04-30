module ShantenTest exposing (suite)

import Data.Group as Group exposing (Group(..))
import Data.Shanten as Shanten
import Data.Tile as Tile exposing (Tile(..))
import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import ShantenTest.Data.Chinitsu as Chinitsu
import ShantenTest.Data.Honitsu as Honitsu
import ShantenTest.Data.Kokushi as Kokushi
import ShantenTest.Data.Standard as Standard
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Data.Shanten module"
        [ describe "Preparation"
            [ test "caseFromString" <|
                \_ ->
                    caseFromString "2 4 8 10 14 15 18 20 22 22 25 30 31 32 4 8 5"
                        |> Expect.equal
                            { tiles = List.filterMap Tile.fromComparable [ 2, 4, 8, 10, 14, 15, 18, 20, 22, 22, 25, 30, 31, 32 ]
                            , shantenStandard = 4
                            , shantenKokushi = 8
                            , shantenChiitoitsu = 5
                            }
            ]
        , describe "shantenStandard"
            [ fuzz oneOfChinitsuData "fuzzingShantenStandard" <|
                \d ->
                    Shanten.shantenStandard d.tiles |> .shanten |> Expect.equal d.shantenStandard
            , describe "shantenStandard 10,000 cases"
                test10000_shantenStandard
            ]
        , describe "shantenKokushi"
            [ fuzz oneOfKokushiData "fuzzingShantenKokushi" <|
                \d ->
                    Shanten.shantenKokushi d.tiles |> Expect.equal d.shantenKokushi

            -- , describe "shantenKokushi 10,000 cases"
            --     test10000_shantenKokushi
            ]
        , describe "shantenChiitoitsu"
            [ fuzz oneOfStandardData "fuzzingShantenChiitoitsu" <|
                \d ->
                    Shanten.shantenChiitoitsu d.tiles |> Expect.equal d.shantenChiitoitsu

            -- , describe "shantenChiitoitsu 10,000 cases"
            --     test10000_shantenChiitoitsu
            ]
        , describe "shantenStandard_debug"
            [ test "3698. 1234579m446789p3s" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialPenchan M1 M2, Run M3 M4 (M5 False), PartialKanchan M7 M9 ], [ Run M1 M2 M3, PartialPenchan M4 (M5 False), PartialKanchan M7 M9 ] ]
                        , pin = [ [ Pair P4 P4, PartialKanchan P6 P8, PartialKanchan P7 P9 ], [ Pair P4 P4, PartialPenchan P6 P7, PartialPenchan P8 P9 ], [ PartialKanchan P4 P6, Run P7 P8 P9 ], [ Pair P4 P4, Run P7 P8 P9 ], [ Pair P4 P4, Run P6 P7 P8 ] ]
                        , sou = []
                        , honor = []
                        }
                        |> Expect.equal 1
            , test "4321. 3555778m2356p35s4z" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialKanchan (M5 False) M7, Pair (M5 False) (M5 False), PartialPenchan M7 M8 ], [ Pair (M5 False) (M5 False), PartialKanchan (M5 False) M7, PartialPenchan M7 M8 ], [ PartialKanchan M3 (M5 False), Pair (M5 False) (M5 False), PartialPenchan M7 M8 ], [ Triplet (M5 False) (M5 False) (M5 False), PartialPenchan M7 M8 ], [ Triplet (M5 False) (M5 False) (M5 False), Pair M7 M7 ] ]
                        , pin = [ [ PartialPenchan P2 P3, PartialPenchan (P5 False) P6 ] ]
                        , sou = [ [ PartialKanchan S3 (S5 False) ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            , test "5762. 2389m16p13445679s" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialPenchan M2 M3, PartialPenchan M8 M9 ] ]
                        , pin = []
                        , sou = [ [ PartialKanchan S1 S3, PartialKanchan S4 S6, PartialPenchan S4 (S5 False), PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, PartialPenchan S4 (S5 False), PartialKanchan S4 S6, PartialKanchan S7 S9 ], [ PartialPenchan S3 S4, Run S4 (S5 False) S6, PartialKanchan S7 S9 ], [ Run S3 S4 (S5 False), PartialKanchan S4 S6, PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, Run S4 (S5 False) S6, PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, Pair S4 S4, PartialPenchan (S5 False) S6, PartialKanchan S7 S9 ], [ PartialKanchan S1 S3, Pair S4 S4, Run (S5 False) S6 S7 ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            , test "6207. 1679m89p12557789s" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialKanchan M7 M9 ], [ PartialPenchan M6 M7 ] ]
                        , pin = [ [ PartialPenchan P8 P9 ] ]
                        , sou = [ [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, PartialKanchan (S5 False) S7, PartialPenchan S8 S9 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, Run S7 S8 S9 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), PartialKanchan S7 S9, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), PartialPenchan S7 S8, PartialKanchan S7 S9 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), Run S7 S8 S9 ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            , test "6212. 234556m13579p167s" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialKanchan M2 M4, PartialKanchan M3 (M5 False), PartialPenchan (M5 False) M6 ], [ PartialPenchan M2 M3, PartialPenchan M4 (M5 False), PartialPenchan (M5 False) M6 ], [ PartialKanchan M3 (M5 False), Run M4 (M5 False) M6 ], [ Run M3 M4 (M5 False), PartialPenchan (M5 False) M6 ], [ PartialPenchan M2 M3, Run M4 (M5 False) M6 ], [ Run M2 M3 M4, PartialPenchan (M5 False) M6 ], [ PartialPenchan M2 M3, PartialKanchan M4 M6, Pair (M5 False) (M5 False) ], [ Run M2 M3 M4, Pair (M5 False) (M5 False) ] ]
                        , pin = [ [ PartialKanchan P3 (P5 False), PartialKanchan P7 P9 ], [ PartialKanchan P1 P3, PartialKanchan P7 P9 ], [ PartialKanchan P1 P3, PartialKanchan (P5 False) P7 ] ]
                        , sou = [ [ PartialPenchan S6 S7 ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            , test "6380. 458m567889p2357s7z" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialPenchan M4 (M5 False) ] ]
                        , pin = [ [ PartialKanchan (P5 False) P7, PartialKanchan P6 P8, PartialPenchan P8 P9 ], [ PartialPenchan (P5 False) P6, PartialPenchan P7 P8, PartialPenchan P8 P9 ], [ PartialKanchan P6 P8, Run P7 P8 P9 ], [ Run P6 P7 P8, PartialPenchan P8 P9 ], [ PartialPenchan (P5 False) P6, Run P7 P8 P9 ], [ Run (P5 False) P6 P7, PartialPenchan P8 P9 ], [ PartialPenchan (P5 False) P6, PartialKanchan P7 P9, Pair P8 P8 ], [ Run (P5 False) P6 P7, Pair P8 P8 ] ]
                        , sou = [ [ PartialPenchan S2 S3, PartialKanchan (S5 False) S7 ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            , test "6896. 6779m12556778s46z" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialPenchan M6 M7, PartialKanchan M7 M9 ], [ Pair M7 M7 ] ]
                        , pin = []
                        , sou = [ [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, PartialKanchan (S5 False) S7, PartialKanchan S6 S8 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, PartialPenchan (S5 False) S6, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, PartialPenchan (S5 False) S6, PartialKanchan (S5 False) S7, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, Run S6 S7 S8 ], [ PartialPenchan S1 S2, PartialKanchan (S5 False) S7, Run (S5 False) S6 S7 ], [ PartialPenchan S1 S2, Run (S5 False) S6 S7, PartialPenchan S7 S8 ], [ PartialPenchan S1 S2, Run (S5 False) S6 S7, PartialKanchan (S5 False) S7 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), PartialKanchan S6 S8, Pair S7 S7 ], [ PartialPenchan S1 S2, Pair (S5 False) (S5 False), Run S6 S7 S8 ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            , test "8190. 479m234556p6889s7z" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialKanchan M7 M9 ] ]
                        , pin = [ [ PartialKanchan P2 P4, PartialKanchan P3 (P5 False), PartialPenchan (P5 False) P6 ], [ PartialPenchan P2 P3, PartialPenchan P4 (P5 False), PartialPenchan (P5 False) P6 ], [ PartialKanchan P3 (P5 False), Run P4 (P5 False) P6 ], [ Run P3 P4 (P5 False), PartialPenchan (P5 False) P6 ], [ PartialPenchan P2 P3, Run P4 (P5 False) P6 ], [ Run P2 P3 P4, PartialPenchan (P5 False) P6 ], [ PartialPenchan P2 P3, PartialKanchan P4 P6, Pair (P5 False) (P5 False) ], [ Run P2 P3 P4, Pair (P5 False) (P5 False) ] ]
                        , sou = [ [ PartialKanchan S6 S8, PartialPenchan S8 S9 ], [ Pair S8 S8 ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            , test "9194. 24m567p122467889s" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialKanchan M2 M4 ] ]
                        , pin = [ [ Run (P5 False) P6 P7 ] ]
                        , sou = [ [ PartialPenchan S1 S2, PartialKanchan S2 S4, PartialKanchan S6 S8, Run S7 S8 S9 ], [ PartialPenchan S1 S2, PartialKanchan S2 S4, Run S6 S7 S8, PartialPenchan S8 S9 ], [ Pair S2 S2, PartialKanchan S4 S6, PartialKanchan S7 S9, Pair S8 S8 ], [ Pair S2 S2, PartialKanchan S6 S8, Run S7 S8 S9 ], [ Pair S2 S2, Run S6 S7 S8, PartialPenchan S8 S9 ], [ Pair S2 S2, PartialKanchan S4 S6, Run S7 S8 S9 ] ]
                        , honor = []
                        }
                        |> Expect.equal 1
            , test "9593. 25668m224456p157s" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ PartialPenchan (M5 False) M6, PartialKanchan M6 M8 ], [ Pair M6 M6 ] ]
                        , pin = [ [ PartialKanchan P2 P4, PartialKanchan P2 P4, PartialPenchan (P5 False) P6 ], [ PartialKanchan P2 P4, Run P4 (P5 False) P6 ], [ Pair P2 P2, PartialKanchan P4 P6, PartialPenchan P4 (P5 False) ], [ Pair P2 P2, PartialPenchan P4 (P5 False), PartialKanchan P4 P6 ], [ Pair P2 P2, Run P4 (P5 False) P6 ] ]
                        , sou = [ [ PartialKanchan (S5 False) S7 ] ]
                        , honor = []
                        }
                        |> Expect.equal 2
            ]
        ]


type alias Case =
    { tiles : List Tile
    , shantenStandard : Int
    , shantenKokushi : Int
    , shantenChiitoitsu : Int
    }


oneOfStandardData : Fuzzer Case
oneOfStandardData =
    List.map Fuzz.constant (String.lines Standard.data)
        |> Fuzz.oneOf
        |> Fuzz.map caseFromString


oneOfKokushiData : Fuzzer Case
oneOfKokushiData =
    List.map Fuzz.constant (String.lines Kokushi.data)
        |> Fuzz.oneOf
        |> Fuzz.map caseFromString


oneOfHonitsuData : Fuzzer Case
oneOfHonitsuData =
    List.map Fuzz.constant (String.lines Honitsu.data)
        |> Fuzz.oneOf
        |> Fuzz.map caseFromString


oneOfChinitsuData : Fuzzer Case
oneOfChinitsuData =
    List.map Fuzz.constant (String.lines Chinitsu.data)
        |> Fuzz.oneOf
        |> Fuzz.map caseFromString


test10000_shantenStandard : List Test
test10000_shantenStandard =
    let
        testShantenStandard index c =
            test (String.fromInt index ++ ". " ++ Tile.tilesToString c.tiles) <|
                \_ -> Shanten.shantenStandard c.tiles |> .shanten |> Expect.equal c.shantenStandard
    in
    casesFromString Standard.data
        |> List.indexedMap testShantenStandard


test10000_shantenKokushi : List Test
test10000_shantenKokushi =
    let
        testShantenKokushi index c =
            test (String.fromInt index ++ ". " ++ Tile.tilesToString c.tiles) <|
                \_ -> Shanten.shantenKokushi c.tiles |> Expect.equal c.shantenKokushi
    in
    casesFromString Kokushi.data
        |> List.indexedMap testShantenKokushi


test10000_shantenChiitoitsu : List Test
test10000_shantenChiitoitsu =
    let
        testShantenChiitoitsu index c =
            test (String.fromInt index ++ ". " ++ Tile.tilesToString c.tiles) <|
                \_ -> Shanten.shantenChiitoitsu c.tiles |> Expect.equal c.shantenChiitoitsu
    in
    casesFromString Standard.data
        |> List.indexedMap testShantenChiitoitsu


casesFromString : String -> List Case
casesFromString str =
    String.lines str
        |> List.map caseFromString


caseFromString : String -> Case
caseFromString str =
    let
        ( tiles, rest ) =
            String.split " " str
                |> List.Extra.splitAt 14
                |> Tuple.mapBoth
                    (List.filterMap (String.toInt >> Maybe.andThen Tile.fromComparable))
                    (List.filterMap String.toInt)

        ( shantenStandard_, shantenKokushi_, shantenChiitoitsu_ ) =
            case rest of
                [ a, b, c ] ->
                    ( a, b, c )

                _ ->
                    ( 0, 0, 0 )
    in
    { tiles = tiles
    , shantenStandard = shantenStandard_
    , shantenKokushi = shantenKokushi_
    , shantenChiitoitsu = shantenChiitoitsu_
    }


shantenStandard_debug :
    { man : List (List Group)
    , pin : List (List Group)
    , sou : List (List Group)
    , honor : List (List Group)
    }
    -> Int
shantenStandard_debug groups =
    let
        groupConfigurations =
            { perSuit = groups }
                |> Group.breakdownCartesianProduct

        completionScores =
            List.map Group.completionScore groupConfigurations
    in
    List.Extra.gatherEquals completionScores
        |> List.map (Tuple.first >> Shanten.completionScoreToShanten 14)
        |> List.minimum
        |> Maybe.withDefault 8
