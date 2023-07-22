module Cell exposing (..)


type ConnectionShape
    = SingleConnection
    | DoubleConnection


type alias Connection1 =
    List ( Int, Int )


type alias Connection2 =
    { moduleId : Int
    , activePos : List ( Int, Int )
    }


type Cell a
    = Glass a
    | Wall
    | Laser
    | Target Bool


map : (a -> b) -> Cell a -> Cell b
map fun cell =
    case cell of
        Glass a ->
            Glass (fun a)

        Wall ->
            Wall

        Laser ->
            Laser

        Target b ->
            Target b


type alias Cell1 =
    Cell Connection1


type alias Cell2 =
    Cell Connection2


cell1ToEmoji : Cell1 -> String
cell1ToEmoji =
    toEmoji
        (\list ->
            case list of
                [] ->
                    "🔲"

                _ ->
                    "🟥"
        )


cell2ToEmoji : Cell2 -> String
cell2ToEmoji =
    toEmoji
        (\connection ->
            case connection.activePos of
                [] ->
                    case connection.moduleId of
                        1 ->
                            "a"

                        2 ->
                            "b"

                        3 ->
                            "c"

                        4 ->
                            "d"

                        _ ->
                            "."

                _ ->
                    case connection.moduleId of
                        1 ->
                            "A"

                        2 ->
                            "B"

                        3 ->
                            "C"

                        4 ->
                            "D"

                        _ ->
                            ":"
        )


toEmoji : (a -> String) -> Cell a -> String
toEmoji fun cell =
    case cell of
        Glass a ->
            fun a

        Wall ->
            "⬛️"

        Laser ->
            "🟥"

        Target False ->
            "🔲"

        Target True ->
            "🟥"


cell1sendsEnergyTo : ( Int, Int ) -> Connection1 -> Bool
cell1sendsEnergyTo to list =
    list |> List.member to


cell2sendsEnergyTo : ( Int, Int ) -> Connection2 -> Bool
cell2sendsEnergyTo to { activePos } =
    activePos |> List.member to
