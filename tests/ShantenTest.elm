module ShantenTest exposing (suite)

import Data.Shanten as Shanten
import Data.Tile as Tile exposing (Category(..), Tile, Value(..))
import Expect
import List.Extra
import ShantenTest.Data.Chinitsu as Chinitsu
import ShantenTest.Data.Honitsu as Honitsu
import ShantenTest.Data.Kokushi as Kokushi
import ShantenTest.Data.Standard as Standard
import Test exposing (Test, describe, test)


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
        , describe "shantenChiitoitsu"
            [ test "6 pairs" <|
                \_ -> Shanten.shantenChiitoitsu (Tile.tilesFromString "225588m11p88s223z") |> Expect.equal 0

            -- , test "2 equal pairs" <|
            --     \_ -> Shanten.shantenChiitoitsu (Tile.tilesFromString "222288m11p88s223z") |> Expect.equal 1
            ]
        , describe "testShantenKokushi"
            (List.indexedMap testShantenKokushi (casesFromString Kokushi.data))
        , describe "testShantenChiitoitsu"
            (List.indexedMap testShantenChiitoitsu (casesFromString Standard.data))
        , describe "testShantenStandard"
            (List.indexedMap testShantenStandard (casesFromString Chinitsu.data))
        ]


testShantenStandard : Int -> Case -> Test
testShantenStandard index q =
    test (String.fromInt index ++ ". " ++ Tile.tilesToString q.tiles) <|
        \_ -> Shanten.shantenStandard q.tiles |> .shanten |> Expect.equal q.shantenStandard


testShantenKokushi : Int -> Case -> Test
testShantenKokushi index q =
    test (String.fromInt index ++ ". " ++ Tile.tilesToString q.tiles) <|
        \_ -> Shanten.shantenKokushi q.tiles |> Expect.equal q.shantenKokushi


testShantenChiitoitsu : Int -> Case -> Test
testShantenChiitoitsu index q =
    test (String.fromInt index ++ ". " ++ Tile.tilesToString q.tiles) <|
        \_ -> Shanten.shantenChiitoitsu q.tiles |> Expect.equal q.shantenChiitoitsu


type alias Case =
    { tiles : List Tile
    , shantenStandard : Int
    , shantenKokushi : Int
    , shantenChiitoitsu : Int
    }


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
