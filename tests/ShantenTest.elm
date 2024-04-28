module ShantenTest exposing (suite)

import Data.Group as Group exposing (Group(..), completionScore)
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
            [ test "0. 4556m33p2234457s1z" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ Run M4 (M5 False) M6 ] ]
                        , pin = [ [ Pair P3 P3 ] ]
                        , sou = [ [ Run S2 S3 S4, PartialKanchan S2 S4, PartialKanchan (S5 False) S7 ], [ Pair S2 S2, Run S3 S4 (S5 False) ] ]
                        , honor = []
                        }
                        |> Expect.equal 1
            , test "5. 7778m23445p456s16z" <|
                \() ->
                    shantenStandard_debug
                        { man = [ [ Triplet M7 M7 M7 ] ]
                        , pin = [ [ Run P2 P3 P4, PartialPenchan P4 (P5 False) ], [ PartialKanchan P2 P4, Run P3 P4 (P5 False) ] ]
                        , sou = [ [ Run S4 (S5 False) S6 ] ]
                        , honor = []
                        }
                        |> Expect.equal 1
            , test "28. 147m234666p56678s" <|
                \() ->
                    shantenStandard_debug
                        { man = []
                        , pin = [ [ Run P2 P3 P4, Triplet P6 P6 P6 ] ]
                        , sou = [ [ Run (S5 False) S6 S7, PartialKanchan S6 S8 ], [ PartialPenchan (S5 False) S6, Run S6 S7 S8 ] ]
                        , honor = []
                        }
                        |> Expect.equal 1
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

        -- TODO are the scores different in some configurations?
        completionScore =
            Group.completionScore (List.head groupConfigurations |> Maybe.withDefault [])

        n =
            if completionScore.pairs > 0 then
                4

            else
                5

        pairs_ =
            if completionScore.pairs > 0 then
                completionScore.pairs - 1

            else
                0

        m =
            min 4 completionScore.groups

        d_ =
            if completionScore.groups + pairs_ + completionScore.partials > 4 then
                4 - m

            else
                pairs_ + completionScore.partials

        g_ =
            14 - (m * 3) - (d_ * 2)

        g =
            if m + d_ + g_ > n then
                n - m - d_

            else
                g_

        d =
            if completionScore.pairs > 0 then
                d_ + 1

            else
                d_
    in
    13 - (m * 3) - (d * 2) - g
