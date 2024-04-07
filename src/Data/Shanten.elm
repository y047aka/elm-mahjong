module Data.Shanten exposing (shantenGuoshi, shantenQidui, shantenStandard)

import Data.Group as Group exposing (Group)
import Data.Tile as Tile exposing (Tile)
import List.Extra


{-|

    import Data.Tile exposing (tilesFromString)

    shantenGuoshi (tilesFromString "19m19p19s1234567z") --> 0
    shantenGuoshi (tilesFromString "19m119p19s134567z") --> 0
    shantenGuoshi (tilesFromString "19m1199p19s14567z") --> 1
    shantenGuoshi (tilesFromString "1m2223334445556s") --> 12
    shantenGuoshi (tilesFromString "19m19p19s12334567z") --> -1

-}
shantenGuoshi : List Tile -> Int
shantenGuoshi tiles =
    let
        counter : List ( Tile, Int )
        counter =
            tiles
                |> List.filter Tile.isYaojiu
                |> List.Extra.gatherEquals
                |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))

        yaojiuCount =
            List.length counter

        hasPair =
            List.any (\( _, count ) -> count >= 2) counter
    in
    if hasPair then
        12 - yaojiuCount

    else
        13 - yaojiuCount


{-|

    import Data.Tile exposing (tilesFromString)

    shantenQidui (tilesFromString "225588m11p88s223z") --> 0
    shantenQidui (tilesFromString "225588m11p88s2233z") --> -1
    shantenQidui (tilesFromString "2268m22334p55s11z4p") --> 0

-}
shantenQidui : List Tile -> Int
shantenQidui tiles =
    let
        counter : List ( Tile, Int )
        counter =
            tiles
                |> List.Extra.gatherEquals
                |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))

        duiziCount =
            List.length <| List.filter (\( _, count ) -> count >= 2) counter

        guliCount =
            List.length <| List.filter (\( _, count ) -> count == 1) counter

        clampedDuiziCount =
            min duiziCount 7

        clampedGuliCount =
            min guliCount (7 - clampedDuiziCount)
    in
    13 - (clampedDuiziCount * 2) - clampedGuliCount


type alias ShantenCalculation =
    { shanten : Int
    , groups : List (List Group)
    }


{-|

    import Data.Tile exposing (tilesFromString)

    shantenStandard (tilesFromString "46789m55779p457s") |> .shanten --> 2
    shantenStandard (tilesFromString "456m567p12388s77z") |> .shanten --> 0
    shantenStandard (tilesFromString "11122456677889p") |> .shanten --> -1
    shantenStandard (tilesFromString "3367m11123p1267s3m") |> .shanten --> 1

-}
shantenStandard : List Tile -> ShantenCalculation
shantenStandard tiles =
    let
        groupConfigurations =
            Group.findGroups Group.FindPartials tiles
                |> Group.breakdownCartesianProduct

        -- TODO are the scores different in some configurations?
        completionScore =
            Group.completionScore (List.head groupConfigurations |> Maybe.withDefault [])

        noPairPenalty =
            let
                usedTiles =
                    3 * completionScore.groups + 2 * (completionScore.pairs + completionScore.partials)

                -- if unusedTiles == 2, any of those tiles is candidate for a pair, after discarding the other one
                unusedTiles =
                    List.length tiles - usedTiles
            in
            if completionScore.pairs == 0 && List.member (List.length tiles) [ 5, 8, 11, 14 ] && unusedTiles /= 2 then
                1

            else
                0

        tooManyGroupsPenalty =
            let
                scoreSum =
                    completionScore.groups + completionScore.pairs + completionScore.partials
            in
            max 0 (scoreSum - 5)

        baselineScore =
            case List.length tiles of
                4 ->
                    2

                5 ->
                    2

                7 ->
                    4

                8 ->
                    4

                10 ->
                    6

                11 ->
                    6

                _ ->
                    8
    in
    { shanten = baselineScore - 2 * completionScore.groups - completionScore.pairs - completionScore.partials + noPairPenalty + tooManyGroupsPenalty
    , groups = groupConfigurations
    }
