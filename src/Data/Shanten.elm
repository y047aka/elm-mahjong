module Data.Shanten exposing
    ( shantenKokushi, shantenChiitoitsu, shantenStandard
    , completionScoreToShanten
    )

{-|

@docs shantenKokushi, shantenChiitoitsu, shantenStandard

-}

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
    in
    { shanten = completionScoreToShanten (List.length tiles) completionScore
    , groups = groupConfigurations
    }


{-|

    completionScoreToShanten 13 { groups = 0, pairs = 0, partials = 0 } --> 8

    七向聴：

    completionScoreToShanten 13 { groups = 0, pairs = 0, partials = 1 } --> 7
    completionScoreToShanten 13 { groups = 0, pairs = 1, partials = 0 } --> 7

    六向聴：

    completionScoreToShanten 13 { groups = 1, pairs = 0, partials = 0 } --> 6

    completionScoreToShanten 13 { groups = 0, pairs = 0, partials = 2 } --> 6
    completionScoreToShanten 13 { groups = 0, pairs = 1, partials = 1 } --> 6
    completionScoreToShanten 13 { groups = 0, pairs = 2, partials = 0 } --> 6

    五向聴：

    completionScoreToShanten 13 { groups = 1, pairs = 0, partials = 1 } --> 5
    completionScoreToShanten 13 { groups = 1, pairs = 1, partials = 0 } --> 5

    completionScoreToShanten 13 { groups = 0, pairs = 0, partials = 3 } --> 5
    completionScoreToShanten 13 { groups = 0, pairs = 1, partials = 2 } --> 5
    completionScoreToShanten 13 { groups = 0, pairs = 2, partials = 1 } --> 5
    completionScoreToShanten 13 { groups = 0, pairs = 3, partials = 0 } --> 5

    四向聴：

    completionScoreToShanten 13 { groups = 2, pairs = 0, partials = 0 } --> 4

    completionScoreToShanten 13 { groups = 1, pairs = 0, partials = 2 } --> 4
    completionScoreToShanten 13 { groups = 1, pairs = 1, partials = 1 } --> 4
    completionScoreToShanten 13 { groups = 1, pairs = 2, partials = 0 } --> 4

    completionScoreToShanten 13 { groups = 0, pairs = 0, partials = 4 } --> 4
    completionScoreToShanten 13 { groups = 0, pairs = 1, partials = 3 } --> 4
    completionScoreToShanten 13 { groups = 0, pairs = 2, partials = 2 } --> 4
    completionScoreToShanten 13 { groups = 0, pairs = 3, partials = 1 } --> 4
    completionScoreToShanten 13 { groups = 0, pairs = 4, partials = 0 } --> 4

    三向聴：

    completionScoreToShanten 13 { groups = 2, pairs = 0, partials = 1 } --> 3
    completionScoreToShanten 13 { groups = 2, pairs = 1, partials = 0 } --> 3

    completionScoreToShanten 13 { groups = 1, pairs = 0, partials = 3 } --> 3
    completionScoreToShanten 13 { groups = 1, pairs = 1, partials = 2 } --> 3
    completionScoreToShanten 13 { groups = 1, pairs = 2, partials = 1 } --> 3
    completionScoreToShanten 13 { groups = 1, pairs = 3, partials = 0 } --> 3

    completionScoreToShanten 13 { groups = 0, pairs = 1, partials = 4 } --> 3
    completionScoreToShanten 13 { groups = 0, pairs = 2, partials = 3 } --> 3
    completionScoreToShanten 13 { groups = 0, pairs = 3, partials = 2 } --> 3
    completionScoreToShanten 13 { groups = 0, pairs = 4, partials = 1 } --> 3
    completionScoreToShanten 13 { groups = 0, pairs = 5, partials = 0 } --> 3

    二向聴：

    completionScoreToShanten 13 { groups = 3, pairs = 0, partials = 0 } --> 2

    completionScoreToShanten 13 { groups = 2, pairs = 0, partials = 2 } --> 2
    completionScoreToShanten 13 { groups = 2, pairs = 1, partials = 1 } --> 2
    completionScoreToShanten 13 { groups = 2, pairs = 2, partials = 0 } --> 2

    completionScoreToShanten 13 { groups = 1, pairs = 1, partials = 3 } --> 2
    completionScoreToShanten 13 { groups = 1, pairs = 2, partials = 2 } --> 2
    completionScoreToShanten 13 { groups = 1, pairs = 3, partials = 1 } --> 2
    completionScoreToShanten 13 { groups = 1, pairs = 4, partials = 0 } --> 2

    一向聴：

    completionScoreToShanten 13 { groups = 3, pairs = 0, partials = 1 } --> 1
    completionScoreToShanten 13 { groups = 3, pairs = 1, partials = 0 } --> 1

    completionScoreToShanten 13 { groups = 2, pairs = 1, partials = 2 } --> 1
    completionScoreToShanten 13 { groups = 2, pairs = 2, partials = 1 } --> 1
    completionScoreToShanten 13 { groups = 2, pairs = 3, partials = 0 } --> 1

    聴牌：

    completionScoreToShanten 13 { groups = 4, pairs = 0, partials = 0 } --> 0

    completionScoreToShanten 13 { groups = 3, pairs = 1, partials = 1 } --> 0
    completionScoreToShanten 13 { groups = 3, pairs = 2, partials = 0 } --> 0

    和了：

    completionScoreToShanten 13 { groups = 4, pairs = 1, partials = 0 } --> -1

-}
completionScoreToShanten : Int -> { groups : Int, pairs : Int, partials : Int } -> Int
completionScoreToShanten tilesLength completionScore =
    let
        hasPair =
            completionScore.pairs > 0

        n =
            if hasPair then
                4

            else
                5

        pairs_ =
            if hasPair then
                completionScore.pairs - 1

            else
                0

        m =
            min 4 completionScore.groups

        d_ =
            min (4 - m) (pairs_ + completionScore.partials)

        g =
            let
                unusedTiles =
                    tilesLength - (m * 3) - (d_ * 2)
            in
            min (n - m - d_) unusedTiles

        d =
            if hasPair then
                d_ + 1

            else
                d_
    in
    13 - (m * 3) - (d * 2) - g
