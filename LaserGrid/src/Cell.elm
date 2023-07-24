module Cell exposing (..)

import Dict exposing (Dict)
import Dir exposing (Dir)
import RelativePos exposing (RelativePos)


type alias ConnectionSort1 =
    { sendsTo : Dict Dir { from : Dir } }


type alias ConnectionSort2 =
    { moduleId : Int
    , rotation : Int
    , sendsTo : Dict RelativePos { from : RelativePos }
    }


type Cell a
    = ConnectionCell a
    | Wall
    | Origin
    | Target (Maybe ( Int, Int ))


map : (a -> b) -> Cell a -> Cell b
map fun cell =
    case cell of
        ConnectionCell connection ->
            ConnectionCell (fun connection)

        Wall ->
            Wall

        Origin ->
            Origin

        Target b ->
            Target b


type alias Cell1 =
    Cell ConnectionSort1


type alias Cell2 =
    Cell ConnectionSort2


cell1ToColor : Maybe Bool -> Cell { connection | sendsTo : Dict a b } -> String
cell1ToColor isActive cell1 =
    case cell1 of
        ConnectionCell sort ->
            case isActive of
                Just True ->
                    "red"

                Just False ->
                    "gray"

                Nothing ->
                    case sort.sendsTo |> Dict.toList of
                        [] ->
                            "gray"

                        _ ->
                            "red"

        Wall ->
            "black"

        Origin ->
            case isActive of
                Just True ->
                    "red"

                Just False ->
                    "gray"

                Nothing ->
                    "red"

        Target Nothing ->
            case isActive of
                Just True ->
                    "red"

                Just False ->
                    "gray"

                Nothing ->
                    "gray"

        Target _ ->
            case isActive of
                Just True ->
                    "red"

                Just False ->
                    "gray"

                Nothing ->
                    "red"
