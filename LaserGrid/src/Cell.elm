module Cell exposing (..)

import Dict exposing (Dict)


type alias ConnectionSort1 =
    ()


type alias ConnectionSort2 =
    { moduleId : Int
    , rotation : Int
    }


type alias Connection a =
    { sendsTo : Dict ( Int, Int ) { from : ( Int, Int ) }
    , sort : a
    }


type Cell a
    = ConnectionCell (Connection a)
    | Wall
    | Origin
    | Target (Maybe ( Int, Int ))


map : (a -> b) -> Cell a -> Cell b
map fun cell =
    case cell of
        ConnectionCell connection ->
            ConnectionCell
                { sendsTo = connection.sendsTo
                , sort = fun connection.sort
                }

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


cell1ToColor : Maybe Bool -> Cell a -> String
cell1ToColor isActive cell1 =
    case cell1 of
        ConnectionCell { sendsTo } ->
            case isActive of
                Just True ->
                    "red"

                Just False ->
                    "gray"

                Nothing ->
                    case sendsTo |> Dict.toList of
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
