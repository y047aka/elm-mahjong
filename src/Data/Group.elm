module Data.Group exposing (FindPartialsOption(..), Group(..), breakdownCartesianProduct, completionScore, findGroups)

import Array
import Data.Counter as Counter
import Data.Tile as Tile exposing (Category(..), Tile)
import List.Extra


type Group
    = Triplet Tile Tile Tile
    | Run Tile Tile Tile
    | Gang Tile Tile Tile Tile
    | Pair Tile Tile
    | PartialPenchan Tile Tile
    | PartialKanchan Tile Tile


type alias GroupsPerSuit =
    { sou : List (List Group)
    , man : List (List Group)
    , pin : List (List Group)
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


findGroups : FindPartialsOption -> List Tile -> GroupsBreakdown
findGroups findPartialGroups tiles =
    let
        part =
            Tile.partitionByCategory tiles
    in
    { perSuit =
        { sou = findGroupsInSuit findPartialGroups Sou part.sou
        , man = findGroupsInSuit findPartialGroups Man part.man
        , pin = findGroupsInSuit findPartialGroups Pin part.pin
        , honor = findGroupsInSuit findPartialGroups Honor part.honor
        }
    }


findGroupsInSuit : FindPartialsOption -> Category -> List Tile -> List (List Group)
findGroupsInSuit findPartialsOption suit tiles =
    List.map Tile.tileToInt tiles
        |> List.sort
        |> Counter.fromIntList
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


keepHighestScore : FindPartialsOption -> List (List Group) -> List (List Group)
keepHighestScore findPartialsOption groups =
    if findPartialsOption == FindPartials then
        let
            groupsWithScores =
                List.map (\g -> ( completionScore g, g )) groups
                    |> List.sortBy (\( cs, _ ) -> ( cs.groups, cs.pairs, cs.partials ))
                    |> List.reverse

            maxScore =
                List.head groupsWithScores
                    |> Maybe.withDefault ( { groups = 0, pairs = 0, partials = 0 }, [] )
                    |> Tuple.first
        in
        List.filter (\( score, _ ) -> score == maxScore) groupsWithScores
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
