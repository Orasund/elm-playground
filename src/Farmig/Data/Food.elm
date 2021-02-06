module Farmig.Data.Food exposing (Food(..), toString, value, waitingTime)


type Food
    = Carrot
    | Berry
    | Cherry
    | Meat


toString : Food -> String
toString food =
    case food of
        Meat ->
            "\u{1F969}"

        Carrot ->
            "\u{1F955}"

        Berry ->
            "\u{1FAD0}"

        Cherry ->
            "🍒"


value : Food -> Int
value food =
    case food of
        Meat ->
            50

        Carrot ->
            30

        Berry ->
            10

        Cherry ->
            5


waitingTime : Food -> Int
waitingTime food =
    case food of
        Meat ->
            0

        Carrot ->
            20

        Berry ->
            5

        Cherry ->
            0
