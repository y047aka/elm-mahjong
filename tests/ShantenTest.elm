module ShantenTest exposing (suite)

import Data.Shanten as Shanten
import Data.Tile as Tile exposing (Category(..), Tile, Value(..))
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
                            { tiles = List.filterMap tilefromInt [ 2, 4, 8, 10, 14, 15, 18, 20, 22, 22, 25, 30, 31, 32 ]
                            , shantenStandard = 4
                            , shantenKokushi = 8
                            , shantenChiitoitsu = 5
                            }
            ]
        , describe "shantenStandard"
            [ fuzz oneOfChinitsuData "fuzzingShantenStandard" <|
                \d ->
                    Shanten.shantenStandard d.tiles |> .shanten |> Expect.equal d.shantenStandard

            -- , describe "shantenStandard 10,000 cases"
            --     test10000_shantenStandard
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
    casesFromString Chinitsu.data
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
                    (List.filterMap (String.toInt >> Maybe.andThen tilefromInt))
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


tilefromInt : Int -> Maybe Tile
tilefromInt n =
    case ( n // 9, remainderBy 9 n ) of
        ( 0, i ) ->
            Maybe.map (\v -> Tile Man v False) (Tile.valueFromInt (i + 1))

        ( 1, i ) ->
            Maybe.map (\v -> Tile Pin v False) (Tile.valueFromInt (i + 1))

        ( 2, i ) ->
            Maybe.map (\v -> Tile Sou v False) (Tile.valueFromInt (i + 1))

        ( 3, 0 ) ->
            Just (Tile Honor East False)

        ( 3, 1 ) ->
            Just (Tile Honor South False)

        ( 3, 2 ) ->
            Just (Tile Honor West False)

        ( 3, 3 ) ->
            Just (Tile Honor North False)

        ( 3, 4 ) ->
            Just (Tile Honor White False)

        ( 3, 5 ) ->
            Just (Tile Honor Green False)

        ( 3, 6 ) ->
            Just (Tile Honor Red False)

        _ ->
            Nothing