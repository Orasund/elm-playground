module Game exposing (..)

import Card exposing (Card)
import Config
import Dict exposing (Dict)
import Game.Evaluate exposing (probabilities)
import Goal exposing (Goal)
import List.Extra
import Random exposing (Generator)
import Random.List
import Set exposing (Set)
import Suit


type alias Random a =
    Generator a


type alias CardId =
    Int


type alias Game =
    { yourCards : Set CardId
    , opponentCards : Set CardId
    , playedCards : List CardId
    , outOfPlay : Set CardId
    , values : Dict String Int
    , cards : Dict CardId Card
    }


fromGoals : Float -> List Goal -> Random Game
fromGoals multiplier list =
    Card.newDeck list
        |> Random.andThen (fromDeck multiplier)


fromDeck : Float -> List Card -> Random Game
fromDeck multiplier sortedDeck =
    Random.List.shuffle sortedDeck
        |> Random.map
            (\deck ->
                let
                    probabilities =
                        deck
                            ++ Card.specialCards
                            |> Game.Evaluate.probabilities
                                { deck =
                                    sortedDeck
                                        |> List.map .suit
                                , open = []
                                }

                    cards =
                        deck
                            |> List.indexedMap Tuple.pair
                            |> Dict.fromList
                in
                { yourCards =
                    cards
                        |> Dict.keys
                        |> List.take Config.cardsPerHand
                        |> Set.fromList
                , opponentCards =
                    cards
                        |> Dict.keys
                        |> List.drop Config.cardsPerHand
                        |> List.take Config.cardsPerHand
                        |> Set.fromList
                , outOfPlay =
                    cards
                        |> Dict.keys
                        |> List.drop (Config.cardsPerHand * 2)
                        |> Set.fromList
                , playedCards = []
                , values = Dict.empty
                , cards = cards
                }
                    |> setValues multiplier probabilities
            )


setValues : Float -> Dict String Int -> Game -> Game
setValues multiplier probabilities game =
    { game
        | values =
            Dict.map
                (\_ probability ->
                    toFloat (100 - probability)
                        * multiplier
                        |> round
                )
                probabilities
    }


getCardFrom : Game -> CardId -> Maybe Card
getCardFrom game id =
    game.cards |> Dict.get id


playCard : CardId -> Game -> Game
playCard cardId game =
    { game
        | yourCards = Set.remove cardId game.yourCards
        , opponentCards = Set.remove cardId game.opponentCards
        , playedCards = cardId :: game.playedCards
    }


isWon : Game -> Bool
isWon game =
    let
        isGoalMet goal =
            game.cards
                |> Dict.toList
                |> List.filterMap
                    (\( id, card ) ->
                        if Set.member id game.outOfPlay then
                            Nothing

                        else
                            Just card.suit
                    )
                |> List.Extra.group
                |> List.map
                    (\( suit, l ) ->
                        ( Suit.icon suit, List.length l + 1 )
                    )
                |> Dict.fromList
                |> Goal.goalMet goal
    in
    game.playedCards
        |> List.head
        |> Maybe.andThen (getCardFrom game)
        |> Maybe.map (\{ goal } -> isGoalMet goal)
        |> Maybe.withDefault False


currentValue : Game -> Int
currentValue game =
    game.playedCards
        |> List.head
        |> Maybe.andThen (getCardFrom game)
        |> Maybe.andThen
            (\card ->
                game.values
                    |> Dict.get (Goal.description card.goal)
            )
        |> Maybe.withDefault 0
