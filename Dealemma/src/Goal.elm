module Goal exposing (..)

import Dict exposing (Dict)
import List.Extra
import Random exposing (Generator)
import Suit exposing (Suit(..))


type alias Random a =
    Generator a


type Category
    = SingleOf Suit
    | PairOf Suit
    | Pair
    | ThreeOf Suit
    | ThreeOfAKind
    | FourOfAKind


type alias Goal =
    List Category


asList : List Goal
asList =
    [ ThreeOfAKind |> List.repeat 2
    , [ FourOfAKind ]
    ]
        ++ (Suit.asList
                |> List.concatMap
                    (\suit ->
                        [ [ PairOf suit, ThreeOfAKind ]
                        , [ ThreeOf suit, Pair ]
                        , [ ThreeOf suit, ThreeOfAKind ]
                        , [ ThreeOf suit ]
                        ]
                    )
           )


special : List Goal
special =
    (Suit.asList
        |> List.concatMap
            (\suit ->
                [ [ PairOf suit ]
                ]
            )
    )
        ++ ([ ( Heart, Diamant )
            , ( Heart, Club )
            , ( Heart, Spade )
            , ( Diamant, Club )
            , ( Diamant, Spade )
            , ( Club, Spade )
            ]
                |> List.concatMap
                    (\( suit1, suit2 ) ->
                        [ [ PairOf suit1, PairOf suit2 ]
                        , [ ThreeOf suit1, PairOf suit2 ]
                        , [ ThreeOf suit2, PairOf suit1 ]
                        , [ ThreeOf suit1, ThreeOf suit2 ]
                        ]
                    )
           )


description : Goal -> String
description list =
    list
        |> List.map categoryDescription
        |> String.join " and "


categoryDescription : Category -> String
categoryDescription category =
    case category of
        SingleOf suit ->
            "one " ++ Suit.icon suit

        PairOf suit ->
            "two " ++ Suit.icon suit

        Pair ->
            "a pair"

        ThreeOf suit ->
            "three " ++ Suit.icon suit

        ThreeOfAKind ->
            "three of a kind"

        FourOfAKind ->
            "four of a kind"


applyCategory : Category -> Dict String Int -> Maybe (Dict String Int)
applyCategory category dict =
    case category of
        SingleOf suit ->
            if Dict.member (Suit.icon suit) dict then
                dict
                    |> Dict.remove (Suit.icon suit)
                    |> Just

            else
                Nothing

        PairOf suit ->
            if
                Dict.get (Suit.icon suit) dict
                    |> Maybe.map (\i -> i >= 2)
                    |> Maybe.withDefault False
            then
                dict
                    |> Dict.remove (Suit.icon suit)
                    |> Just

            else
                Nothing

        Pair ->
            Suit.asList
                |> List.Extra.find
                    (\suit ->
                        dict
                            |> Dict.get (Suit.icon suit)
                            |> Maybe.map (\i -> i >= 2)
                            |> Maybe.withDefault False
                    )
                |> Maybe.map (\suit -> Dict.remove (Suit.icon suit) dict)

        ThreeOf suit ->
            if
                Dict.get (Suit.icon suit) dict
                    |> Maybe.map (\i -> i >= 3)
                    |> Maybe.withDefault False
            then
                dict
                    |> Dict.remove (Suit.icon suit)
                    |> Just

            else
                Nothing

        ThreeOfAKind ->
            Suit.asList
                |> List.Extra.find
                    (\suit ->
                        dict
                            |> Dict.get (Suit.icon suit)
                            |> Maybe.map (\i -> i >= 3)
                            |> Maybe.withDefault False
                    )
                |> Maybe.map (\suit -> Dict.remove (Suit.icon suit) dict)

        FourOfAKind ->
            Suit.asList
                |> List.Extra.find
                    (\suit ->
                        dict
                            |> Dict.get (Suit.icon suit)
                            |> Maybe.map (\i -> i >= 4)
                            |> Maybe.withDefault False
                    )
                |> Maybe.map (\suit -> Dict.remove (Suit.icon suit) dict)


goalMet : Goal -> Dict String Int -> Bool
goalMet list dict =
    list
        |> List.foldl
            (\category ->
                Maybe.andThen (applyCategory category)
            )
            (Just dict)
        |> (/=) Nothing
