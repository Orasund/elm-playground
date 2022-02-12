module Depp.Data.Deck exposing (Deck, faceToInt, faceToString, isTrump, new, suitToInt, suitToString)

import Array exposing (Array)
import Cards exposing (Face(..), Suit(..))
import Random exposing (Generator)
import Random.List


type alias Deck =
    List ( Suit, Face )


isTrump : Suit -> Bool
isTrump =
    (==) Hearts


suitToString : Suit -> String
suitToString suit =
    case suit of
        Spades ->
            "♠"

        Diamonds ->
            "♦"

        Clubs ->
            "♣"

        Hearts ->
            "♥"


suitToInt : Suit -> Int
suitToInt suit =
    case suit of
        Spades ->
            0

        Diamonds ->
            1

        Clubs ->
            2

        Hearts ->
            3


suits : Array Suit
suits =
    [ Spades
    , Diamonds
    , Clubs
    , Hearts
    ]
        |> Array.fromList


faceToInt : Face -> Int
faceToInt face =
    case face of
        Ace ->
            0

        Two ->
            1

        Three ->
            2

        Four ->
            3

        Five ->
            4

        Six ->
            5

        Seven ->
            6

        Eight ->
            7

        Nine ->
            8

        Ten ->
            9

        Jack ->
            10

        Queen ->
            11

        King ->
            12


faceToString : Face -> String
faceToString face =
    case face of
        Ace ->
            "A"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"


faces : Array Face
faces =
    [ Ace
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    ]
        |> Array.fromList


new : Generator Deck
new =
    suits
        |> Array.toList
        |> List.concatMap
            (\suit ->
                List.range 0 9
                    |> List.filterMap (\i -> faces |> Array.get i)
                    |> List.map (\face -> ( suit, face ))
            )
        |> Random.List.shuffle
