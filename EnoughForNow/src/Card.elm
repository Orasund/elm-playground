module Card exposing (..)


type alias CardId =
    Int


type Card
    = Wood
    | Stone
    | Food


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


name : Card -> String
name card =
    case card of
        Wood ->
            "Wood"

        Stone ->
            "Stone"

        Food ->
            "Food"


emoji : Card -> String
emoji card =
    case card of
        Wood ->
            "\u{1FAB5}"

        Stone ->
            "\u{1FAA8}"

        Food ->
            "\u{1FAD0}"
