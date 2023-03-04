module Card exposing (..)


type alias CardId =
    Int


type Card
    = Wood
    | Fire
    | Food
    | Fear


values : List Card
values =
    [ Fear, Wood, Food, Fire ]


type alias CardType =
    Int


cardType : Card -> CardType
cardType card =
    case card of
        Food ->
            0

        Fire ->
            1

        Wood ->
            2

        Fear ->
            3


name : Card -> String
name card =
    case card of
        Wood ->
            "Wood"

        Fire ->
            "Fire"

        Food ->
            "Food"

        Fear ->
            "Fear"


emoji : Card -> String
emoji card =
    case card of
        Wood ->
            "\u{1FAB5}"

        Fire ->
            "🔥"

        Food ->
            "\u{1FAD0}"

        Fear ->
            "💀"
