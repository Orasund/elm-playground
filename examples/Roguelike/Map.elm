module Roguelike.Map exposing (Location, Map, dirCoordinates, generate, getUnique, place, remove)

import Dict exposing (Dict)
import Random
import Roguelike.Cell as Cell exposing (Direction(..))


type alias Location =
    ( Int, Int )


type alias Map a =
    Dict Location a


generate : Int -> (Location -> ( Map a, Random.Seed ) -> ( Map a, Random.Seed )) -> Random.Seed -> ( Map a, Random.Seed )
generate size fun seed =
    List.range 0 size
        |> List.foldl
            (\x out ->
                List.range 0 size
                    |> List.foldl
                        (\y ( map, seed ) -> fun ( x, y ) ( map, seed ))
                        out
            )
            ( Dict.empty, seed )


dirCoordinates : Direction -> ( Int, Int )
dirCoordinates direction =
    case direction of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( 1, 0 )

        Right ->
            ( -1, 0 )


place : Location -> a -> Map a -> Map a
place location cell map =
    map
        |> Dict.update location (always (Just cell))


remove : Location -> Map a -> Map a
remove location map =
    map |> Dict.remove location


getUnique : (Location -> a -> Bool) -> Map a -> Maybe ( Location, a )
getUnique fun map =
    map
        |> Dict.filter fun
        |> Dict.toList
        |> List.head
