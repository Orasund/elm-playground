module Suit exposing (..)


type Suit
    = Heart
    | Diamant
    | Spade
    | Club


asList : List Suit
asList =
    [ Heart
    , Diamant
    , Spade
    , Club
    ]


icon : Suit -> String
icon suit =
    case suit of
        Heart ->
            "♥"

        Diamant ->
            "♦"

        Spade ->
            "♠"

        Club ->
            "♣"
