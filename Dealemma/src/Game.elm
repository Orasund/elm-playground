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
    , probabilities : Dict String Int
    , cards : Dict CardId Card
    }


fromGoals : List Goal -> Random Game
fromGoals list =
    Card.newDeck list
        |> Random.andThen fromDeck


fromDeck : List Card -> Random Game
fromDeck sortedDeck =
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
                , probabilities = probabilities
                , cards = cards
                }
            )


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


currentPercentage : Game -> Int
currentPercentage game =
    game.playedCards
        |> List.head
        |> Maybe.andThen (getCardFrom game)
        |> Maybe.andThen
            (\card ->
                game.probabilities
                    |> Dict.get (Goal.description card.goal)
            )
        |> Maybe.withDefault 100
