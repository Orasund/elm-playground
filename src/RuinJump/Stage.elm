module RuinJump.Stage exposing (Stage, placeStairs, removeN)

import Dict
import List.Zipper as Zipper exposing (Zipper)
import Random exposing (Generator, Seed)
import RuinJump.Map as Map exposing (Map)
import RuinJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuinJump.MapSlice as MapSlice


type alias Stage =
    { map : Map
    , lowestY : Int
    , currentY : Int
    , xSlice : Zipper Int
    , decaySpeed : Int
    }


placeStairs : ( Int, Int ) -> ( Int, Int ) -> Stage -> Generator Stage
placeStairs pos1 pos2 ( { map, decaySpeed } as stage) =
    (MapElement.woodGenerator
            |> Random.andThen
                (\e1 ->
                    MapElement.woodGenerator
                        |> Random.map
                            (\e2 ->
                                { stage
                                    | map =
                                        map
                                            |> Dict.insert pos2 e1
                                            |> Dict.insert pos1 e2
                                    , decaySpeed = decaySpeed + 1
                                }
                            )
                )
        )


removeN : Int -> ( Stage, Seed ) -> ( Stage, Seed )
removeN decaySpeed ( stage, seed ) =
    List.range 1 decaySpeed
        |> List.foldl
            (always
                (\( m, s ) ->
                    s
                        |> Random.step
                            (m |> removeOne)
                )
            )
            ( stage, seed )


removeOne : Stage -> Random.Generator Stage
removeOne ({ xSlice, lowestY, map } as stage) =
    let
        x : Int
        x =
            xSlice |> Zipper.current
    in
    case xSlice |> Zipper.next of
        Just slice ->
            Random.constant
                { stage
                    | xSlice = slice
                    , map = map |> Map.remove ( x, lowestY )
                }

        Nothing ->
            MapSlice.generator (lowestY - 1) map
                |> Random.map
                    (\slice ->
                        { stage
                            | xSlice = slice
                            , lowestY = lowestY - 1
                            , map = map |> Map.remove ( x, lowestY )
                        }
                    )
