module Roguelike.Player exposing (PlayerData, face, get)

import Roguelike.Cell as Cell exposing (Cell(..), Direction(..), Item)
import Roguelike.Map as Map exposing (Map)


type alias PlayerData =
    ( Map.Location, Direction )


get : Map Cell -> Maybe PlayerData
get map =
    map
        |> Map.getUnique
            (\key cell ->
                case cell of
                    Player _ ->
                        True

                    _ ->
                        False
            )
        |> Maybe.andThen
            (\( key, cell ) ->
                case cell of
                    Player dir ->
                        Just ( key, dir )

                    _ ->
                        Nothing
            )


face : Map.Location -> Direction -> Map Cell -> Map Cell
face location direction map =
    map |> Map.place location (Player direction)
