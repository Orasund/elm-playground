module Data.Sound exposing (..)


type Sound
    = Mine
    | PickUp
    | Unload
    | MovingTrain


asList : List Sound
asList =
    [ Mine, PickUp, Unload, MovingTrain ]


toFile : Sound -> String
toFile sound =
    case sound of
        Mine ->
            "mine.mp3"

        PickUp ->
            "pickup.mp3"

        Unload ->
            "unload.mp3"

        MovingTrain ->
            "movingTrain.mp3"


toString : Sound -> String
toString sound =
    case sound of
        Mine ->
            "Mine"

        PickUp ->
            "PickUp"

        Unload ->
            "Unload"

        MovingTrain ->
            "MovingTrain"
