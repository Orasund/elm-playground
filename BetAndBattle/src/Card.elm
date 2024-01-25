module Card exposing (..)

import Dict exposing (Dict)


type Card
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


asList : List Card
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


goalDescription : Card -> String
goalDescription card =
    case card of
        OneOne ->
            "One 1"

        Street ->
            "All numbers 1 to 4"

        NoFour ->
            "No four"

        NoFourAndNoThree ->
            "No four and no three"

        NoFourOrNoThree ->
            "No four or no three"

        PairOfTwo ->
            "Two 2"

        PairOfTwoAndPairOfOne ->
            "Pair of 2 and pair of 1"

        ThreePairs ->
            "Three pairs"

        PairOfTwoAndOnePair ->
            "Two 2 and another pair"

        PairOfTwoAndTwoPairs ->
            "Two 2 and two other pairs"

        Tripple ->
            "Three of a kind"

        TrippleOfThree ->
            "Three 3"

        TrippleAndPairOfTwo ->
            "Three of a kind and a pair of 2"

        TrippleOfThreeAndPairOfTwo ->
            "Three 3 and a pair of 2"

        TwoTripples ->
            "Two different three of a kind"

        Quadripple ->
            "Four of a kind"

        QuadrippleOfFour ->
            "Four 4"

        QuadrippleAndTripple ->
            "Four of a kind and three of a kind"


goalMet : Card -> Dict Int Int -> Bool
goalMet card dict =
    case card of
        OneOne ->
            Dict.member 1 dict

        Street ->
            Dict.member 1 dict
                && Dict.member 2 dict
                && Dict.member 3 dict
                && Dict.member 4 dict

        NoFour ->
            Dict.member 4 dict |> not

        NoFourAndNoThree ->
            not (Dict.member 4 dict)
                && not (Dict.member 3 dict)

        PairOfTwo ->
            Dict.get 2 dict
                |> Maybe.map (\i -> i >= 2)
                |> Maybe.withDefault False

        PairOfTwoAndOnePair ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d -> Dict.member 2 d && Dict.size d >= 2)

        PairOfTwoAndPairOfOne ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d -> Dict.member 2 d && Dict.member 1 d)

        ThreePairs ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d -> Dict.size d >= 3)

        PairOfTwoAndTwoPairs ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d -> Dict.member 2 d && Dict.size d >= 3)

        Tripple ->
            dict
                |> Dict.filter (\_ i -> i >= 3)
                |> (\d -> Dict.size d >= 1)

        TrippleOfThree ->
            dict
                |> Dict.get 3
                |> Maybe.map (\i -> i >= 3)
                |> Maybe.withDefault False

        TrippleAndPairOfTwo ->
            (dict
                |> Dict.filter (\_ i -> i >= 3)
                |> (\d -> Dict.size d >= 1)
            )
                && (dict
                        |> Dict.get 2
                        |> Maybe.map (\i -> i >= 2)
                        |> Maybe.withDefault False
                   )

        TrippleOfThreeAndPairOfTwo ->
            (dict
                |> Dict.get 3
                |> Maybe.map (\i -> i >= 3)
                |> Maybe.withDefault False
            )
                && (dict
                        |> Dict.get 2
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
                |> Dict.get 3
                |> Maybe.map (\i -> i >= 4)
                |> Maybe.withDefault False

        NoFourOrNoThree ->
            not (Dict.member 4 dict)
                || not (Dict.member 3 dict)

        QuadrippleAndTripple ->
            (dict
                |> Dict.filter (\_ i -> i >= 4)
                |> (\d -> Dict.size d >= 1)
            )
                && (dict
                        |> Dict.filter (\_ i -> i >= 3)
                        |> (\d -> Dict.size d >= 1)
                   )


probability : Card -> Int
probability card =
    case card of
        NoFourAndNoThree ->
            1

        QuadrippleOfFour ->
            36

        NoFour ->
            46

        NoFourOrNoThree ->
            87

        TrippleOfThreeAndPairOfTwo ->
            144

        QuadrippleAndTripple ->
            154

        Quadripple ->
            154

        TwoTripples ->
            253

        TrippleOfThree ->
            261

        PairOfTwoAndPairOfOne ->
            487

        PairOfTwoAndTwoPairs ->
            580

        TrippleAndPairOfTwo ->
            616

        PairOfTwoAndOnePair ->
            725

        PairOfTwo ->
            725

        ThreePairs ->
            748

        Street ->
            845

        Tripple ->
            891

        OneOne ->
            973
