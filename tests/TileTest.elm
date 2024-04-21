module TileTest exposing (suite)

import Data.Tile as Tile exposing (Category(..), Value(..))
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz)


tilesString : Fuzzer String
tilesString =
    [ suitString "m", suitString "p", suitString "s", honorString ]
        |> Fuzz.sequence
        |> Fuzz.map String.concat


suitString : String -> Fuzzer String
suitString category =
    (Fuzz.oneOf <| List.map Fuzz.constant [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ])
        |> Fuzz.listOfLengthBetween 0 4
        |> Fuzz.map
            (\tiles ->
                if tiles == [] then
                    ""

                else
                    (List.sort tiles |> String.concat) ++ category
            )


honorString : Fuzzer String
honorString =
    (Fuzz.oneOf <| List.map Fuzz.constant [ "1", "2", "3", "4", "5", "6", "7" ])
        |> Fuzz.listOfLengthBetween 0 4
        |> Fuzz.map
            (\tiles ->
                if tiles == [] then
                    ""

                else
                    (List.sort tiles |> String.concat) ++ "z"
            )


suite : Test
suite =
    describe "Data.Tile module"
        [ fuzz tilesString "tilesFromString and tilesToString" <|
            \str ->
                Tile.tilesFromString str
                    |> Tile.tilesToString
                    |> Expect.equal str
        ]
