module Level exposing (..)

import Cell exposing (Cell(..))
import Dict exposing (Dict)


type alias Level =
    ( Dict ( Int, Int ) Cell, List ( Int, Int ) )


empty : Dict ( Int, Int ) Cell
empty =
    List.range -1 4
        |> List.concatMap (\i -> [ ( i, -1 ), ( i, 4 ) ])
        |> (++) (List.range 0 3 |> List.concatMap (\i -> [ ( -1, i ), ( 4, i ) ]))
        |> List.map (\pos -> ( pos, Wall ))
        |> Dict.fromList


fromInt : Int -> Level
fromInt int =
    case int of
        2 ->
            ( empty
                |> withLaserAt ( 0, -1 )
                |> withLaserAt ( 1, -1 )
            , []
            )
                |> withTargetAt ( 2, 4 )
                |> withTargetAt ( 1, 4 )

        _ ->
            ( empty
                |> withLaserAt ( 0, -1 )
            , []
            )
                |> withTargetAt ( 3, 4 )


withLaserAt : ( Int, Int ) -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
withLaserAt pos =
    Dict.insert pos Laser


withTargetAt : ( Int, Int ) -> Level -> Level
withTargetAt pos ( grid, list ) =
    ( grid |> Dict.insert pos (Target False), pos :: list )
