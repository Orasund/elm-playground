module Goal exposing (..)

import Dict exposing (Dict)
import Suit exposing (Suit(..))


type Goal
    = OneOne
    | Street
    | NoFour
    | NoFourAndNoThree
    | NoFourOrNoThree
    | PairOfTwo
    | PairOfTwoAndPairOfOne
    | PairOfTwoAndOnePair
    | PairOfTwoAndTwoPairs
    | ThreePairs
    | Tripple
    | TrippleOfThree
    | TrippleAndPairOfTwo
    | TrippleOfThreeAndPairOfTwo
    | TwoTripples
    | Quadripple
    | QuadrippleOfFour
    | QuadrippleAndTripple


asList : List Goal
asList =
    [ OneOne
    , Street
    , NoFour
    , NoFourAndNoThree
    , PairOfTwo
    , PairOfTwoAndPairOfOne
    , ThreePairs
    , PairOfTwoAndOnePair
    , PairOfTwoAndTwoPairs
    , Tripple
    , TrippleOfThree
    , TrippleAndPairOfTwo
    , TrippleOfThreeAndPairOfTwo
    , TwoTripples
    , Quadripple
    , QuadrippleOfFour
    , NoFourOrNoThree
    , QuadrippleAndTripple
    ]


goalDescription : Goal -> String
goalDescription card =
    case card of
        OneOne ->
            "One " ++ Suit.icon Heart

        Street ->
            "Four different suits"

        NoFour ->
            "No four"

        NoFourAndNoThree ->
            "No four and no three"

        NoFourOrNoThree ->
            "No four or no three"

        PairOfTwo ->
            "Two " ++ Suit.icon Diamant

        PairOfTwoAndPairOfOne ->
            "Pair of "
                ++ Suit.icon Diamant
                ++ " and pair of "
                ++ Suit.icon Heart

        ThreePairs ->
            "Three pairs"

        PairOfTwoAndOnePair ->
            "Two " ++ Suit.icon Diamant ++ " and another pair"

        PairOfTwoAndTwoPairs ->
            "Two " ++ Suit.icon Diamant ++ " and two other pairs"

        Tripple ->
            "Three of a kind"

        TrippleOfThree ->
            "Three " ++ Suit.icon Spade

        TrippleAndPairOfTwo ->
            "Three of a kind and a pair of " ++ Suit.icon Diamant

        TrippleOfThreeAndPairOfTwo ->
            "Three " ++ Suit.icon Spade ++ " and a pair of " ++ Suit.icon Diamant

        TwoTripples ->
            "Two different three of a kind"

        Quadripple ->
            "Four of a kind"

        QuadrippleOfFour ->
            "Four " ++ Suit.icon Club

        QuadrippleAndTripple ->
            "Four of a kind and three of a kind"


goalMet : Goal -> Dict String Int -> Bool
goalMet card dict =
    case card of
        OneOne ->
            Dict.member (Suit.icon Heart) dict

        Street ->
            Suit.asList
                |> List.all
                    (\suit ->
                        Dict.member (Suit.icon suit) dict
                    )

        NoFour ->
            Dict.member (Suit.icon Club) dict |> not

        NoFourAndNoThree ->
            not (Dict.member (Suit.icon Club) dict)
                && not (Dict.member (Suit.icon Spade) dict)

        PairOfTwo ->
            Dict.get (Suit.icon Diamant) dict
                |> Maybe.map (\i -> i >= 2)
                |> Maybe.withDefault False

        PairOfTwoAndOnePair ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d ->
                        Dict.member (Suit.icon Diamant) d
                            && Dict.size d
                            >= 2
                   )

        PairOfTwoAndPairOfOne ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d ->
                        Dict.member (Suit.icon Diamant) d
                            && Dict.member (Suit.icon Heart) d
                   )

        ThreePairs ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d -> Dict.size d >= 3)

        PairOfTwoAndTwoPairs ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d -> Dict.member (Suit.icon Diamant) d && Dict.size d >= 3)

        Tripple ->
            dict
                |> Dict.filter (\_ i -> i >= 3)
                |> (\d -> Dict.size d >= 1)

        TrippleOfThree ->
            dict
                |> Dict.get (Suit.icon Spade)
                |> Maybe.map (\i -> i >= 3)
                |> Maybe.withDefault False

        TrippleAndPairOfTwo ->
            (dict
                |> Dict.filter (\_ i -> i >= 3)
                |> (\d -> Dict.size d >= 1)
            )
                && (dict
                        |> Dict.get (Suit.icon Diamant)
                        |> Maybe.map (\i -> i >= 2)
                        |> Maybe.withDefault False
                   )

        TrippleOfThreeAndPairOfTwo ->
            (dict
                |> Dict.get (Suit.icon Spade)
                |> Maybe.map (\i -> i >= 3)
                |> Maybe.withDefault False
            )
                && (dict
                        |> Dict.get (Suit.icon Diamant)
                        |> Maybe.map (\i -> i >= 2)
                        |> Maybe.withDefault False
                   )

        TwoTripples ->
            dict
                |> Dict.filter (\_ i -> i >= 3)
                |> (\d -> Dict.size d >= 2)

        Quadripple ->
            dict
                |> Dict.filter (\_ i -> i >= 4)
                |> (\d -> Dict.size d >= 1)

        QuadrippleOfFour ->
            dict
                |> Dict.get (Suit.icon Spade)
                |> Maybe.map (\i -> i >= 4)
                |> Maybe.withDefault False

        NoFourOrNoThree ->
            not (Dict.member (Suit.icon Club) dict)
                || not (Dict.member (Suit.icon Spade) dict)

        QuadrippleAndTripple ->
            (dict
                |> Dict.filter (\_ i -> i >= 4)
                |> (\d -> Dict.size d >= 1)
            )
                && (dict
                        |> Dict.filter (\_ i -> i >= 3)
                        |> (\d -> Dict.size d >= 1)
                   )


probability : Goal -> Int
probability card =
    case card of
        NoFourAndNoThree ->
            0

        QuadrippleOfFour ->
            8

        NoFour ->
            6

        NoFourOrNoThree ->
            12

        TrippleOfThreeAndPairOfTwo ->
            17

        QuadrippleAndTripple ->
            18

        Quadripple ->
            18

        TwoTripples ->
            28

        TrippleOfThree ->
            36

        PairOfTwoAndPairOfOne ->
            45

        PairOfTwoAndTwoPairs ->
            58

        TrippleAndPairOfTwo ->
            63

        PairOfTwoAndOnePair ->
            73

        PairOfTwo ->
            73

        ThreePairs ->
            73

        Street ->
            81

        Tripple ->
            90

        OneOne ->
            96
