module Data.Shanten exposing (shantenChiitoitsu, shantenKokushi, shantenStandard)

import Data.Group as Group exposing (Group)
import Data.Tile as Tile exposing (Tile)


{-|

    import Data.Tile exposing (tilesFromString)

    shantenKokushi (tilesFromString "19m19p19s1234567z") --> 0
    shantenKokushi (tilesFromString "19m119p19s134567z") --> 0
    shantenKokushi (tilesFromString "19m1199p19s14567z") --> 1
    shantenKokushi (tilesFromString "1m2223334445556s") --> 12
    shantenKokushi (tilesFromString "19m19p19s12334567z") --> -1

-}
shantenKokushi : List Tile -> Int
shantenKokushi tiles =
    let
        counter =
            tiles
                |> List.filter Tile.isYaojiu
                |> Tile.countTiles

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

    shantenChiitoitsu (tilesFromString "225588m11p88s223z") --> 0
    shantenChiitoitsu (tilesFromString "222288m11p88s223z") --> 2
    shantenChiitoitsu (tilesFromString "225588m11p88s2233z") --> -1
    shantenChiitoitsu (tilesFromString "2268m22334p55s11z4p") --> 0

-}
shantenChiitoitsu : List Tile -> Int
shantenChiitoitsu tiles =
    let
        counter =
            Tile.countTiles tiles

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
            List.length tiles - (m * 3) - (d_ * 2)

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
    { shanten = 13 - (m * 3) - (d * 2) - g
    , groups = groupConfigurations
    }
