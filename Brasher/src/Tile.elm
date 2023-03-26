module Tile exposing (..)


type alias TileId =
    Int


type Tile
    = Enemy
    | AxeThrower
    | Player
    | Chair
    | Door
    | Axe
    | Shield
    | Money


emoji : Tile -> String
emoji tile =
    case tile of
        Enemy ->
            "🤺"

        AxeThrower ->
            "🤾"

        Player ->
            "😎"

        Door ->
            "🚪"

        Chair ->
            "🪑"

        Axe ->
            "🪓"

        Shield ->
            "🛋"

        Money ->
            "💰"


description : Tile -> String
description tile =
    case tile of
        Enemy ->
            "kick the guard"

        AxeThrower ->
            "kick the axe thrower"

        Player ->
            "hi"

        Chair ->
            "smash the chair"

        Door ->
            "kick down the door"

        Axe ->
            "catch the axe"

        Shield ->
            "take cover behind the sofa"

        Money ->
            "grab the money"
