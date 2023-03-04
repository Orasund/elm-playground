module Card exposing (..)


type alias CardId =
    Int


type Card
    = Wood
    | Stone
    | Food
    | Fear


values : List Card
values =
    [ Food, Stone, Wood, Fear ]


type alias CardType =
    Int


cardType : Card -> CardType
cardType card =
    case card of
        Food ->
            0

        Stone ->
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

        Stone ->
            "Stone"

        Food ->
            "Food"

        Fear ->
            "Fear"


emoji : Card -> String
emoji card =
    case card of
        Wood ->
            "\u{1FAB5}"

        Stone ->
            "\u{1FAA8}"

        Food ->
            "\u{1FAD0}"

        Fear ->
            "💀"
