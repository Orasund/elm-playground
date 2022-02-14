module Depp.Data.Rule exposing (ClubsRule(..), DiamondsRule(..), HeartsRule(..), Rule(..), defaultRules, toString)

import Cards exposing (Suit(..))
import Depp.Config as Config
import Depp.Data.Deck as Deck
import Dict.Any as AnyDict exposing (AnyDict)
import Set.Any as AnySet exposing (AnySet)


type HeartsRule
    = HaveSameValue


type ClubsRule
    = HighestStaysInHand


type DiamondsRule
    = MaySwapWithSameValue


type Rule
    = HeartsRule HeartsRule
    | ClubsRule ClubsRule
    | DiamondsRule DiamondsRule


toString : Rule -> String
toString rule =
    case rule of
        HeartsRule r ->
            "Hearts "
                ++ (case r of
                        HaveSameValue ->
                            "have a value of " ++ String.fromInt Config.heartsRuleSameValue ++ "."
                   )

        ClubsRule r ->
            "Clubs "
                ++ (case r of
                        HighestStaysInHand ->
                            "stay in your hand, if they are the highest clubs you got."
                   )

        DiamondsRule r ->
            "Diamonds "
                ++ (case r of
                        MaySwapWithSameValue ->
                            "can be swapped with cards of same value"
                   )


defaultRules : AnyDict String Rule Suit
defaultRules =
    [ ( HeartsRule HaveSameValue, Hearts )
    , ( ClubsRule HighestStaysInHand, Clubs )
    , ( DiamondsRule MaySwapWithSameValue, Diamonds )
    ]
        |> AnyDict.fromList toString
