module HermeticMind.Data.Alphabet exposing (TwentySix, english, german, twentySix)

import StaticArray.Index as Index exposing (Five, Index, OnePlus, TwentyPlus)
import StaticArray.Length as Length exposing (Length)


type alias TwentySix =
    TwentyPlus (OnePlus Five)


twentySix : Length TwentySix
twentySix =
    Length.five |> Length.plus1 |> Length.plus20


german : Char -> Index TwentySix
german char =
    (case char |> Char.toLower of
        'e' ->
            1

        'n' ->
            2

        'i' ->
            3

        's' ->
            4

        'r' ->
            5

        'a' ->
            6

        't' ->
            7

        'd' ->
            8

        'h' ->
            9

        'u' ->
            10

        'l' ->
            11

        'c' ->
            12

        'g' ->
            13

        'm' ->
            14

        'o' ->
            15

        'b' ->
            16

        'w' ->
            17

        'f' ->
            18

        'k' ->
            19

        'z' ->
            20

        'p' ->
            21

        'v' ->
            22

        'j' ->
            23

        'y' ->
            24

        'x' ->
            25

        'q' ->
            26

        _ ->
            0
    )
        |> (-) 1
        |> (*) 7
        |> (+) 13
        |> Index.fromModBy twentySix


english : Char -> Index TwentySix
english char =
    (case char |> Char.toLower of
        'e' ->
            1

        'a' ->
            2

        'r' ->
            3

        'i' ->
            4

        'o' ->
            5

        't' ->
            6

        'n' ->
            7

        's' ->
            8

        'l' ->
            9

        'c' ->
            10

        'u' ->
            11

        'd' ->
            12

        'p' ->
            13

        'm' ->
            14

        'h' ->
            15

        'g' ->
            16

        'b' ->
            17

        'f' ->
            18

        'y' ->
            19

        'w' ->
            20

        'k' ->
            21

        'v' ->
            22

        'x' ->
            23

        'z' ->
            24

        'j' ->
            25

        'q' ->
            26

        _ ->
            0
    )
        |> (-) 1
        |> (*) 7
        |> (+) 13
        |> Index.fromModBy twentySix
