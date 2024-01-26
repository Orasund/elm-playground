module Suit exposing (..)


type Suit
    = Heart
    | Diamant
    | Spade
    | Club
    | Star


asList : List Suit
asList =
    [ Heart
    , Diamant
    , Spade
    , Club

    --, Star
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

        Star ->
            "★"


color : Suit -> String
color suit =
    case suit of
        Heart ->
            "#F9C09F"

        Diamant ->
            "#D6F6CB"

        Spade ->
            "#98CAEC"

        Club ->
            "#A2E2AA"

        Star ->
            "#FA9EC1"
