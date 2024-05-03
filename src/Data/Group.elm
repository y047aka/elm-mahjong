module Data.Group exposing (FindPartialsOption(..), Group(..), breakdownCartesianProduct, completionScore, consumePair, consumePartialKanchan, consumePartialRyanmenPenchan, consumeRun, consumeTriplet, findGroups, findGroupsInSuit, keepHighestScore)

import Array
import Data.Category exposing (Category(..))
import Data.Counter as Counter
import Data.Tile as Tile exposing (Tile)
import List.Extra


type Group
    = Triplet Tile Tile Tile
    | Run Tile Tile Tile
    | Gang Tile Tile Tile Tile
    | Pair Tile Tile
    | PartialPenchan Tile Tile
    | PartialKanchan Tile Tile
    | Kokushi Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile


type alias GroupsPerSuit =
    { man : List (List Group)
    , pin : List (List Group)
    , sou : List (List Group)
    , honor : List (List Group)
    }


type alias GroupsBreakdown =
    { perSuit : GroupsPerSuit }


type FindPartialsOption
    = FindPartials
    | SkipPartials


type alias CompletionScore =
    { groups : Int
    , pairs : Int
    , partials : Int
    }


{-|

    findGroups FindPartials []
    --> { perSuit = { sou = [], man = [], pin = [], honor = [] } }

-}
findGroups : FindPartialsOption -> List Tile -> GroupsBreakdown
findGroups findPartialGroups tiles =
    let
        part =
            Tile.partitionByCategory tiles
    in
    { perSuit =
        { man = findGroupsInSuit findPartialGroups Man part.man
        , pin = findGroupsInSuit findPartialGroups Pin part.pin
        , sou = findGroupsInSuit findPartialGroups Sou part.sou
        , honor = findGroupsInSuit findPartialGroups Honor part.honor
        }
    }


{-|

    import Data.Category exposing (Category(..))

    findGroupsInSuit FindPartials Man [] --> []

-}
findGroupsInSuit : FindPartialsOption -> Category -> List Tile -> List (List Group)
findGroupsInSuit findPartialsOption suit tiles =
    Counter.fromTileList tiles
        |> findGroupsInSuitHelper findPartialsOption suit 0 True
        |> Maybe.withDefault []
        |> List.Extra.remove []
        |> keepHighestScore findPartialsOption


findGroupsInSuitHelper : FindPartialsOption -> Category -> Int -> Bool -> Counter.Counter -> Maybe (List (List Group))
findGroupsInSuitHelper findPartialsOption suit n shouldFindPair counter =
    let
        count =
            Counter.getCount n counter
    in
    if n >= Array.length counter then
        Just [ [] ]

    else if count == 0 then
        findGroupsInSuitHelper findPartialsOption suit (n + 1) shouldFindPair counter

    else
        let
            triplet =
                consumeTriplet findPartialsOption suit n shouldFindPair counter count

            pair =
                consumePair findPartialsOption suit n shouldFindPair counter count

            run =
                consumeRun findPartialsOption suit n shouldFindPair counter count

            partial1 =
                consumePartialRyanmenPenchan findPartialsOption suit n shouldFindPair counter count

            partial2 =
                consumePartialKanchan findPartialsOption suit n shouldFindPair counter count

            -- skip isolated tiles
            skipTile =
                if findPartialsOption == FindPartials && count == 1 then
                    findGroupsInSuitHelper findPartialsOption suit (n + 1) shouldFindPair counter

                else
                    Nothing
        in
        map2RetainJust List.append triplet run
            |> map2RetainJust List.append pair
            |> map2RetainJust List.append partial1
            |> map2RetainJust List.append partial2
            |> map2RetainJust List.append skipTile


{-|

    import Data.Category exposing (Category(..))
    import Data.Counter as Counter
    import Data.Tile exposing (Tile(..))

    consumeRun FindPartials Honor 0 True (Counter.fromTileList []) 0 --> Nothing
    consumeRun FindPartials Man 0 True (Counter.fromTileList [ M2, M3 ]) 1 --> Just [[Run M1 M2 M3]]

-}
consumeRun : FindPartialsOption -> Category -> Int -> Bool -> Counter.Counter -> Int -> Maybe (List (List Group))
consumeRun findPartialsOption suit n shouldFindPair counter count =
    let
        foundRun =
            suit /= Honor && n < 7 && count >= 1 && Counter.getCount (n + 1) counter > 0 && Counter.getCount (n + 2) counter > 0
    in
    if foundRun then
        let
            count2 =
                Counter.getCount (n + 1) counter

            count3 =
                Counter.getCount (n + 2) counter

            updatedCounter =
                counter
                    |> Array.set n (count - 1)
                    |> Array.set (n + 1) (count2 - 1)
                    |> Array.set (n + 2) (count3 - 1)
        in
        findGroupsInSuitHelper findPartialsOption suit n shouldFindPair updatedCounter
            |> Maybe.map2 addGroupToHead
                (Maybe.map3 Run
                    (Tile.compose ( suit, n + 1 ))
                    (Tile.compose ( suit, n + 2 ))
                    (Tile.compose ( suit, n + 3 ))
                )

    else
        Nothing


{-|

    import Data.Category exposing (Category(..))
    import Data.Counter as Counter
    import Data.Tile exposing (Tile(..))

    consumePair FindPartials Honor 0 True (Counter.fromTileList []) 0 --> Nothing
    consumePair FindPartials Man 0 True (Counter.fromTileList []) 2 --> Just [ [ Pair M1 M1 ] ]

-}
consumePair : FindPartialsOption -> Category -> Int -> Bool -> Counter.Counter -> Int -> Maybe (List (List Group))
consumePair findPartialsOption suit n shouldFindPair counter count =
    if count >= 2 && (shouldFindPair || findPartialsOption == FindPartials) then
        findGroupsInSuitHelper findPartialsOption suit n False (Array.set n (count - 2) counter)
            |> Maybe.map2 addGroupToHead
                (Maybe.map2 Pair
                    (Tile.compose ( suit, n + 1 ))
                    (Tile.compose ( suit, n + 1 ))
                )

    else
        Nothing


{-|

    import Data.Category exposing (Category(..))
    import Data.Counter as Counter
    import Data.Tile exposing (Tile(..))

    consumeTriplet FindPartials Honor 0 True (Counter.fromTileList []) 0 --> Nothing
    consumeTriplet FindPartials Man 0 True (Counter.fromTileList []) 3 --> Just [[Triplet M1 M1 M1]]

-}
consumeTriplet : FindPartialsOption -> Category -> Int -> Bool -> Counter.Counter -> Int -> Maybe (List (List Group))
consumeTriplet findPartialsOption suit n shouldFindPair counter count =
    if count >= 3 then
        findGroupsInSuitHelper findPartialsOption suit n shouldFindPair (Array.set n (count - 3) counter)
            |> Maybe.map2 addGroupToHead
                (Maybe.map3 Triplet
                    (Tile.compose ( suit, n + 1 ))
                    (Tile.compose ( suit, n + 1 ))
                    (Tile.compose ( suit, n + 1 ))
                )

    else
        Nothing


{-|

    import Data.Category exposing (Category(..))
    import Data.Counter as Counter

    consumePartialRyanmenPenchan FindPartials Honor 0 True (Counter.fromTileList []) 0 --> Nothing

-}
consumePartialRyanmenPenchan : FindPartialsOption -> Category -> Int -> Bool -> Counter.Counter -> Int -> Maybe (List (List Group))
consumePartialRyanmenPenchan findPartialsOption suit n shouldFindPair counter count =
    let
        count2 =
            Counter.getCount (n + 1) counter
    in
    if findPartialsOption == FindPartials && suit /= Honor && count >= 1 && count2 >= 1 then
        let
            updatedCounter =
                counter
                    |> Array.set n (count - 1)
                    |> Array.set (n + 1) (count2 - 1)
        in
        findGroupsInSuitHelper findPartialsOption suit n shouldFindPair updatedCounter
            |> Maybe.map2 addGroupToHead
                (Maybe.map2 PartialPenchan
                    (Tile.compose ( suit, n + 1 ))
                    (Tile.compose ( suit, n + 2 ))
                )

    else
        Nothing


{-|

    import Data.Category exposing (Category(..))
    import Data.Counter as Counter

    consumePartialKanchan FindPartials Honor 0 True (Counter.fromTileList []) 0 --> Nothing

-}
consumePartialKanchan : FindPartialsOption -> Category -> Int -> Bool -> Counter.Counter -> Int -> Maybe (List (List Group))
consumePartialKanchan findPartialsOption suit n shouldFindPair counter count =
    let
        count2 =
            Counter.getCount (n + 2) counter
    in
    if findPartialsOption == FindPartials && suit /= Honor && count >= 1 && count2 >= 1 then
        let
            updatedCounter =
                counter
                    |> Array.set n (count - 1)
                    |> Array.set (n + 2) (count2 - 1)
        in
        findGroupsInSuitHelper findPartialsOption suit n shouldFindPair updatedCounter
            |> Maybe.map2 addGroupToHead
                (Maybe.map2 PartialKanchan
                    (Tile.compose ( suit, n + 1 ))
                    (Tile.compose ( suit, n + 3 ))
                )

    else
        Nothing


{-|

    import Data.Tile exposing (Tile(..))

    keepHighestScore FindPartials [] --> []
    keepHighestScore FindPartials [ [ Triplet M1 M1 M1 ] ] --> [ [ Triplet M1 M1 M1 ] ]
    keepHighestScore FindPartials [ [ Triplet M1 M1 M1 ], [ PartialKanchan M1 M3 ] ] --> [ [ Triplet M1 M1 M1 ] ]

-}
keepHighestScore : FindPartialsOption -> List (List Group) -> List (List Group)
keepHighestScore findPartialsOption groups =
    if findPartialsOption == FindPartials then
        let
            usedTiles cs =
                (cs.groups * 3) + (cs.pairs * 2) + (cs.partials * 2)

            partitionByCompletionScore =
                List.map (\g -> ( completionScore g, g )) groups
                    |> List.Extra.gatherEqualsBy Tuple.first

            groupsA =
                partitionByCompletionScore
                    |> List.Extra.minimumBy (\( ( cs, _ ), _ ) -> ( 14 - usedTiles cs, cs.pairs + cs.partials ))

            groupsB =
                partitionByCompletionScore
                    |> List.Extra.maximumBy (\( ( cs, _ ), _ ) -> ( cs.groups, cs.pairs + cs.partials ))

            groupC =
                partitionByCompletionScore
                    |> List.filter (\( ( cs, _ ), _ ) -> cs.pairs > 0)
                    |> List.Extra.minimumBy (\( ( cs, _ ), _ ) -> ( 14 - usedTiles cs, cs.pairs + cs.partials ))

            groupD =
                partitionByCompletionScore
                    |> List.filter (\( ( cs, _ ), _ ) -> cs.pairs > 0)
                    |> List.Extra.maximumBy (\( ( cs, _ ), _ ) -> ( cs.groups, cs.pairs + cs.partials ))
        in
        [ groupsA, groupsB, groupC, groupD ]
            |> List.filterMap identity
            |> List.Extra.uniqueBy (\( ( cs, _ ), _ ) -> cs)
            |> List.concatMap (\( head, tails ) -> head :: tails)
            |> List.map Tuple.second

    else
        -- there are only complete groups and pairs, no need to sort
        groups



-- using Maybe.map2 with only 1 Nothing just retains the Nothing


map2RetainJust : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
map2RetainJust func a b =
    case ( a, b ) of
        ( Nothing, bb ) ->
            bb

        ( aa, Nothing ) ->
            aa

        ( Just aa, Just bb ) ->
            Maybe.map2 func (Just aa) (Just bb)


addGroupToHead : Group -> List (List Group) -> List (List Group)
addGroupToHead group foundGroups =
    List.map ((::) group) foundGroups


isPair : Group -> Bool
isPair group =
    case group of
        Pair _ _ ->
            True

        _ ->
            False


breakdownCartesianProduct : GroupsBreakdown -> List (List Group)
breakdownCartesianProduct groups =
    [ groups.perSuit.man, groups.perSuit.pin, groups.perSuit.sou, groups.perSuit.honor ]
        |> List.filter (\lg -> not (List.isEmpty lg))
        |> List.Extra.cartesianProduct
        |> List.map List.concat


isPartial : Group -> Bool
isPartial group =
    case group of
        Triplet _ _ _ ->
            False

        Run _ _ _ ->
            False

        Gang _ _ _ _ ->
            False

        Pair _ _ ->
            False

        PartialPenchan _ _ ->
            True

        PartialKanchan _ _ ->
            True

        Kokushi _ _ _ _ _ _ _ _ _ _ _ _ _ _ ->
            False


{-|

    import Data.Tile exposing (Tile(..))

    completionScore []
    --> { groups = 0, pairs = 0, partials = 0 }

    completionScore [ ( Triplet M1 M1 M1 ), ( Triplet East East East ) ]
    --> { groups = 2, pairs = 0, partials = 0 }

    completionScore [ ( Pair M1 M1 ), ( Pair East East ) ]
    --> { groups = 0, pairs = 2, partials = 0 }

    completionScore [ ( PartialPenchan M1 M2 ), ( PartialKanchan M7 M9 ) ]
    --> { groups = 0, pairs = 0, partials = 2 }

-}
completionScore : List Group -> CompletionScore
completionScore groups =
    let
        calc group acc =
            if isPartial group then
                { acc | partials = acc.partials + 1 }

            else if isPair group then
                { acc | pairs = acc.pairs + 1 }

            else
                { acc | groups = acc.groups + 1 }
    in
    List.foldl calc (CompletionScore 0 0 0) groups
