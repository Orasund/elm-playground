module Data.Sound exposing (..)


type Sound
    = Mine
    | PickUp


asList : List Sound
asList =
    [ Mine, PickUp ]


toFile : Sound -> String
toFile sound =
    case sound of
        Mine ->
            "footstep_concrete_000.ogg"

        PickUp ->
            "select_006.ogg"


toString : Sound -> String
toString sound =
    case sound of
        Mine ->
            "Mine"

        PickUp ->
            "PickUp"
