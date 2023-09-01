module Crop exposing (..)


type Crop
    = Tomato
    | Clover
    | Carrot
    | Beans


toEmoji : Crop -> String
toEmoji crop =
    case crop of
        Tomato ->
            "🍅"

        Clover ->
            "☘️"

        Carrot ->
            "🥕"

        Beans ->
            "\u{1FADB}"


badNeighbors : Crop -> { bad : List Crop }
badNeighbors crop =
    case crop of
        Beans ->
            { bad = [ Tomato ] }

        Carrot ->
            { bad = [] }

        Tomato ->
            { bad = [] }

        Clover ->
            { bad = [] }


maxAge : Crop -> Int
maxAge crop =
    case crop of
        Tomato ->
            6

        Clover ->
            2

        Carrot ->
            6

        Beans ->
            2


price : Crop -> Int
price crop =
    case crop of
        Tomato ->
            3

        Clover ->
            0

        Carrot ->
            2

        Beans ->
            1


soilHealthNeeded : Crop -> Int
soilHealthNeeded crop =
    let
        low =
            1

        middle =
            2

        high =
            4
    in
    case crop of
        Tomato ->
            high

        Clover ->
            0

        Carrot ->
            middle

        Beans ->
            low
