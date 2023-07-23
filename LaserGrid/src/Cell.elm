module Cell exposing (..)


type alias ConnectionSort1 =
    ()


type alias ConnectionSort2 =
    { moduleId : Int
    , rotation : Int
    }


type alias Connection a =
    { active : List ( Int, Int ), sort : a }


type Cell a
    = ConnectionCell (Connection a)
    | Wall
    | Laser
    | Target Bool


map : (a -> b) -> Cell a -> Cell b
map fun cell =
    case cell of
        ConnectionCell connection ->
            ConnectionCell
                { active = connection.active
                , sort = fun connection.sort
                }

        Wall ->
            Wall

        Laser ->
            Laser

        Target b ->
            Target b


type alias Cell1 =
    Cell ConnectionSort1


type alias Cell2 =
    Cell ConnectionSort2


cell1ToColor : Cell a -> String
cell1ToColor cell1 =
    case cell1 of
        ConnectionCell { active } ->
            case active of
                [] ->
                    "gray"

                _ ->
                    "red"

        Wall ->
            "black"

        Laser ->
            "red"

        Target False ->
            "gray"

        Target True ->
            "red"


connectionSendsEnergyTo : ( Int, Int ) -> Connection a -> Bool
connectionSendsEnergyTo to { active } =
    active |> List.member to
