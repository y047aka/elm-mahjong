module Data.Group exposing (Group(..))

import Data.Tile as Tile exposing (Tile)
import List.Extra


type Group
    = Triplet Tile Tile Tile
    | Run Tile Tile Tile
    | Gang Tile Tile Tile Tile
    | Pair Tile Tile
    | Penchan Tile Tile
    | Kanchan Tile Tile


findGroups : List Tile -> List (List Group)
findGroups tiles =
    tiles
        |> List.Extra.permutations
        |> List.map (List.Extra.greedyGroupsOf 3 >> List.filterMap findGroup)


findGroup : List Tile -> Maybe Group
findGroup tiles =
    case tiles of
        t1 :: t2 :: [] ->
            fromTuple2 ( t1, t2 )

        t1 :: t2 :: t3 :: [] ->
            fromTuple3 ( t1, t2, t3 )

        t1 :: t2 :: t3 :: t4 :: [] ->
            if Tile.isGang t1 t2 t3 t4 then
                Just (Gang t1 t2 t3 t4)

            else
                Nothing

        _ ->
            Nothing


fromTuple3 : ( Tile, Tile, Tile ) -> Maybe Group
fromTuple3 (( t1, t2, t3 ) as tuple3) =
    if Tile.isTriplet tuple3 then
        Just (Triplet t1 t2 t3)

    else if Tile.isRun tuple3 then
        Just (Run t1 t2 t3)

    else
        Nothing


fromTuple2 : ( Tile, Tile ) -> Maybe Group
fromTuple2 (( t1, t2 ) as tuple2) =
    if Tile.isPair tuple2 then
        Just (Pair t1 t2)

    else if Tile.isPenchan tuple2 then
        Just (Penchan t1 t2)

    else if Tile.isKanchan tuple2 then
        Just (Kanchan t1 t2)

    else
        Nothing


uniqueDuizis : List Tile -> List ( Tile, Tile )
uniqueDuizis tiles =
    tiles
        |> List.Extra.gatherEquals
        |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))
        |> List.filter (\( _, count ) -> count >= 2)
        |> List.map (\( t, _ ) -> ( t, t ))


uniquePengs : List Tile -> List ( Tile, Tile, Tile )
uniquePengs tiles =
    tiles
        |> List.Extra.gatherEquals
        |> List.map (\( head, tails ) -> ( head, 1 + List.length tails ))
        |> List.filter (\( _, count ) -> count >= 3)
        |> List.map (\( t, _ ) -> ( t, t, t ))
