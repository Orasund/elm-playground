module Cell exposing (..)


type Cell
    = Glass (Maybe { from : ( Int, Int ), to : ( Int, Int ) })
    | Wall
    | Laser
    | Target Bool


toEmoji : Cell -> String
toEmoji cell =
    case cell of
        Glass Nothing ->
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
