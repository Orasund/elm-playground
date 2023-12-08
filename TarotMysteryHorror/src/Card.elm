module Card exposing (..)


type Card
    = Door
    | Death
    | Letter
    | Raven


name : Card -> String
name card =
    case card of
        Door ->
            "Die Tür"

        Death ->
            "Der Tod"

        Letter ->
            "Der Brief"

        Raven ->
            "Der Rabe"
