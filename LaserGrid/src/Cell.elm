module Cell exposing (..)


type Cell
    = Glass (List ( Int, Int ))
    | Wall
    | Laser
    | Target Bool


toEmoji : Cell -> String
toEmoji cell =
    case cell of
        Glass [] ->
            "🔲"

        Glass _ ->
            "🟥"

        Wall ->
            "⬛️"

        Laser ->
            "🟥"

        Target False ->
            "🔲"

        Target True ->
            "🟥"
