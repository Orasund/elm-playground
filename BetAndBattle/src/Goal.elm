module Goal exposing (..)

import Config
import Dict exposing (Dict)
import List.Extra
import Random exposing (Generator)
import Random.List
import Suit exposing (Suit(..))


type alias Random a =
    Generator a


type Goal
    = --Single Suit
      --|
      PairOf Suit
    | OnePairAndOnePairOf Suit
    | TwoPairsAndOnePairOf Suit
    | Street
      --| NoFour
      --| NoFourAndNoThree
      --| NoFourOrNoThree
    | PairOfTwoAndPairOfOne
    | ThreePairs
    | Tripple
    | TrippleOfThree
    | TrippleAndPairOfTwo
    | TrippleOfThreeAndPairOfTwo
    | TwoTripples
    | Quadripple



--| QuadrippleOfFour
--| QuadrippleAndTripple


asList : List Goal
asList =
    [ Street

    --, NoFour
    -- , NoFourAndNoThree
    , PairOfTwoAndPairOfOne
    , ThreePairs
    , Tripple
    , TrippleOfThree
    , TrippleAndPairOfTwo
    , TrippleOfThreeAndPairOfTwo
    , TwoTripples
    , Quadripple

    --, QuadrippleOfFour
    -- , NoFourOrNoThree
    --, QuadrippleAndTripple
    ]
        ++ (Suit.asList
                |> List.concatMap
                    (\suit ->
                        [ -- Single
                          --,
                          PairOf
                        , OnePairAndOnePairOf
                        , TwoPairsAndOnePairOf
                        ]
                            |> List.map (\f -> f suit)
                    )
           )


goalDescription : Goal -> String
goalDescription card =
    case card of
        --Single suit ->
        --    "One " ++ Suit.icon suit
        Street ->
            "Four different suits"

        --NoFour ->
        --    "No four"
        --NoFourAndNoThree ->
        --    "No four and no three"
        --NoFourOrNoThree ->
        --    "No four or no three"
        PairOf suit ->
            "Two " ++ Suit.icon suit

        PairOfTwoAndPairOfOne ->
            "Pair of "
                ++ Suit.icon Diamant
                ++ " and pair of "
                ++ Suit.icon Heart

        ThreePairs ->
            "Three pairs"

        OnePairAndOnePairOf suit ->
            "Two " ++ Suit.icon suit ++ " and another pair"

        TwoPairsAndOnePairOf suit ->
            "Two " ++ Suit.icon suit ++ " and two other pairs"

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



--QuadrippleOfFour ->
--    "Four " ++ Suit.icon Club
--QuadrippleAndTripple ->
--    "Four of a kind and three of a kind"


goalMet : Goal -> Dict String Int -> Bool
goalMet card dict =
    case card of
        --Single suit ->
        --    Dict.member (Suit.icon suit) dict
        Street ->
            Suit.asList
                |> List.all
                    (\suit ->
                        Dict.member (Suit.icon suit) dict
                    )

        --NoFour ->
        --    Dict.member (Suit.icon Club) dict |> not
        --NoFourAndNoThree ->
        --    not (Dict.member (Suit.icon Club) dict)
        --        && not (Dict.member (Suit.icon Spade) dict)
        PairOf suit ->
            Dict.get (Suit.icon suit) dict
                |> Maybe.map (\i -> i >= 2)
                |> Maybe.withDefault False

        OnePairAndOnePairOf suit ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d ->
                        Dict.member (Suit.icon suit) d
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

        TwoPairsAndOnePairOf suit ->
            dict
                |> Dict.filter (\_ i -> i >= 2)
                |> (\d -> Dict.member (Suit.icon suit) d && Dict.size d >= 3)

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



--QuadrippleOfFour ->
--    dict
--       |> Dict.get (Suit.icon Spade)
--        |> Maybe.map (\i -> i >= 4)
--        |> Maybe.withDefault False
--NoFourOrNoThree ->
--    not (Dict.member (Suit.icon Club) dict)
--        || not (Dict.member (Suit.icon Spade) dict)
--QuadrippleAndTripple ->
--    (dict
--        |> Dict.filter (\_ i -> i >= 4)
--        |> (\d -> Dict.size d >= 1)
--    )
--        && (dict
--                |> Dict.filter (\_ i -> i >= 3)
--                |> (\d -> Dict.size d >= 1)
--           )


probability : Goal -> Int
probability goal =
    Dict.get (goalDescription goal) probabilities
        |> Maybe.withDefault 0


probabilities : Dict String Int
probabilities =
    let
        randomDeck : Random (List Suit)
        randomDeck =
            Suit.asList
                |> List.concatMap (List.repeat Config.cardsPerSuit)
                |> Random.List.shuffle
                |> Random.map (List.take (Config.cardsPerHand * 2))

        simulateGame : Goal -> List Suit -> Bool
        simulateGame card list =
            list
                |> List.Extra.gatherEquals
                |> List.map
                    (\( suit, l ) ->
                        ( Suit.icon suit, List.length l + 1 )
                    )
                |> Dict.fromList
                |> goalMet card

        ( decks, _ ) =
            randomDeck
                |> Random.list 100
                |> (\random -> Random.step random (Random.initialSeed 42))
    in
    asList
        |> List.map
            (\goal ->
                ( goalDescription goal
                , decks
                    |> List.map (simulateGame goal)
                    |> List.Extra.count identity
                )
            )
        |> Dict.fromList
