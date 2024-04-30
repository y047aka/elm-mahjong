module ShantenTest exposing (suite)

import Data.Shanten as Shanten exposing (ShantenSummary)
import Data.Tile as Tile exposing (Tile)
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
                            , standard = 4
                            , kokushi = 8
                            , chiitoitsu = 5
                            , minimum = 4
                            }
            ]
        , describe "shantenSummary"
            [ fuzz oneOfStandardData "fuzzingStandardData" <|
                \d ->
                    Shanten.shantenSummary d.tiles
                        |> Expect.equal (caseToShantenSummary d)
            , fuzz oneOfKokushiData "fuzzingKokushiData" <|
                \d ->
                    Shanten.shantenSummary d.tiles
                        |> Expect.equal (caseToShantenSummary d)
            , fuzz oneOfHonitsuData "fuzzingHonitsuData" <|
                \d ->
                    Shanten.shantenSummary d.tiles
                        |> Expect.equal (caseToShantenSummary d)
            , fuzz oneOfChinitsuData "fuzzingChinitsuData" <|
                \d ->
                    Shanten.shantenSummary d.tiles
                        |> Expect.equal (caseToShantenSummary d)
            ]

        -- , describe "shantenSummary 10,000 cases"
        --     [ describe "Standard.data" <|
        --         test10000 Standard.data
        --     , describe "Kokushi.data" <|
        --         test10000 Kokushi.data
        --     , describe "Honitsu.data" <|
        --         test10000 Honitsu.data
        --     , describe "Chinitsu.data" <|
        --         test10000 Chinitsu.data
        --     ]
        ]


type alias Case =
    { tiles : List Tile
    , standard : Int
    , chiitoitsu : Int
    , kokushi : Int
    , minimum : Int
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


caseToShantenSummary : Case -> ShantenSummary
caseToShantenSummary { standard, kokushi, chiitoitsu, minimum } =
    { standard = standard
    , kokushi = kokushi
    , chiitoitsu = chiitoitsu
    , minimum = minimum
    }


test10000 : String -> List Test
test10000 data =
    let
        test_ index c =
            test (String.fromInt index ++ ". " ++ Tile.tilesToString c.tiles) <|
                \_ -> Shanten.shantenSummary c.tiles |> Expect.equal (caseToShantenSummary c)
    in
    String.lines data
        |> List.map caseFromString
        |> List.indexedMap test_


caseFromString : String -> Case
caseFromString str =
    let
        ( tiles, shantenValues ) =
            String.split " " str
                |> List.Extra.splitAt 14
                |> Tuple.mapBoth
                    (List.filterMap (String.toInt >> Maybe.andThen Tile.fromComparable))
                    (List.filterMap String.toInt)

        ( standard, kokushi, chiitoitsu ) =
            case shantenValues of
                [ a, b, c ] ->
                    ( a, b, c )

                _ ->
                    ( 0, 0, 0 )
    in
    { tiles = tiles
    , standard = standard
    , chiitoitsu = chiitoitsu
    , kokushi = kokushi
    , minimum =
        List.minimum [ standard, kokushi, chiitoitsu ]
            |> Maybe.withDefault 8
    }
