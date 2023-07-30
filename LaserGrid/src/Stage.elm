module Stage exposing (..)

import Cell exposing (Cell(..))
import Dict exposing (Dict)


type alias Stage =
    { grid : Dict ( Int, Int ) Cell
    , targets : List ( Int, Int )
    }


isSolved : Stage -> Bool
isSolved stage =
    List.all
        (\pos ->
            case stage.grid |> Dict.get pos of
                Just (Target (Just _)) ->
                    True

                _ ->
                    False
        )
        stage.targets


fromDict : Dict ( Int, Int ) Cell -> Stage
fromDict dict =
    { grid = dict
    , targets =
        dict
            |> Dict.filter
                (\_ cell ->
                    case cell of
                        Target _ ->
                            True

                        _ ->
                            False
                )
            |> Dict.keys
    }


parse : List String -> Stage
parse rows =
    rows
        |> List.indexedMap
            (\y string ->
                string
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            let
                                pos =
                                    ( x - 1, y - 1 )
                            in
                            case char of
                                '🟥' ->
                                    Just ( pos, Origin )

                                '🔘' ->
                                    Just ( pos, Target Nothing )

                                '⬛' ->
                                    Just ( pos, Wall )

                                _ ->
                                    Nothing
                        )
                    |> List.filterMap identity
            )
        |> List.concat
        |> Dict.fromList
        |> fromDict


withLaserAt : ( Int, Int ) -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
withLaserAt pos =
    Dict.insert pos Origin
