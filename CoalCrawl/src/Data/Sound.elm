module Data.Sound exposing (..)


type Sound
    = Build
    | PickUp
    | Unload
    | MovingTrain
    | Error


asList : List Sound
asList =
    [ Build, PickUp, Unload, MovingTrain, Error ]


toFile : Sound -> String
toFile sound =
    case sound of
        Build ->
            "build.mp3"

        PickUp ->
            "pickup.mp3"

        Unload ->
            "unload.mp3"

        MovingTrain ->
            "movingTrain.mp3"

        Error ->
            "error.mp3"


toString : Sound -> String
toString sound =
    case sound of
        Build ->
            "Build"

        PickUp ->
            "PickUp"

        Unload ->
            "Unload"

        MovingTrain ->
            "MovingTrain"

        Error ->
            "Error"
