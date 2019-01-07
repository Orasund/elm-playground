module RuineJump.MapSlice exposing (fromMap)

import Dict exposing (Dict)
import List.Zipper as Zipper exposing (Zipper)
import Random

getWeights : List a -> Random.Seed -> ( List Int, Random.Seed )
getWeights list oldSeed =
    Random.step
        (Random.list
            (list |> List.length)
            (Random.int Random.minInt Random.maxInt)
        )
        oldSeed

fromMap : Int -> Random.Seed -> Dict ( Int, Int ) a -> ( Zipper Int, Random.Seed )
fromMap lowestY seed map =
    let
        orderedSlice =
            map
                |> Dict.filter (\( _, y ) _ -> y == lowestY)
                |> Dict.toList

        ( weights, newSeed ) =
            getWeights orderedSlice seed

        shuffledSlice =
            orderedSlice
                |> List.map2
                    (\s ( ( x, _ ), _ ) -> ( x, s ))
                    weights
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
                |> Zipper.fromList
                |> Zipper.withDefault 0
    in
    ( shuffledSlice, newSeed )