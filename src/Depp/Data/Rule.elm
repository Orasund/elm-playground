module Depp.Data.Rule exposing (Rule(..), defaultRules, toString)

import Cards exposing (Suit(..))
import Depp.Config as Config
import Depp.Data.Deck as Deck
import Dict.Any as AnyDict exposing (AnyDict)
import Set.Any as AnySet exposing (AnySet)


type Rule
    = HaveSameValue
    | HighestStaysInHand
    | MaySwapWithSameValue
    | ReduceValueIfLower


toComparable : Rule -> Int
toComparable rule =
    case rule of
        HaveSameValue ->
            1

        HighestStaysInHand ->
            2

        MaySwapWithSameValue ->
            3

        ReduceValueIfLower ->
            4


toString : ( Rule, Suit ) -> String
toString ( rule, suit ) =
    (suit |> Deck.suitToString)
        ++ " "
        ++ (case rule of
                HaveSameValue ->
                    "has a value of " ++ String.fromInt Config.heartsRuleSameValue ++ "."

                HighestStaysInHand ->
                    "stays in your hand, if it is the highest of this suit that you got."

                MaySwapWithSameValue ->
                    "can be swapped with cards of same value."

                ReduceValueIfLower ->
                    "will reduce the value of cards it can not beat."
           )


defaultRules : AnyDict Int Rule Suit
defaultRules =
    [ ( HaveSameValue, Hearts )
    , ( HighestStaysInHand, Clubs )
    , ( MaySwapWithSameValue, Diamonds )
    , ( ReduceValueIfLower, Spades )
    ]
        |> AnyDict.fromList toComparable
