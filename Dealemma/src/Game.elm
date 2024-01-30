module Game exposing (..)

import Card exposing (Card)
import Config
import Dict exposing (Dict)
import Game.Evaluate exposing (probabilities)
import Goal exposing (Goal)
import Random exposing (Generator)
import Random.List
import Suit


type alias Random a =
    Generator a


type alias Game =
    { yourCards : List Card
    , opponentCards : List Card
    , playedCards : List Card
    , probabilities : Dict String Int
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
                                }
                in
                { yourCards =
                    List.take Config.cardsPerHand deck
                        |> List.sortBy
                            (\card ->
                                probabilities
                                    |> Dict.get (Goal.description card.goal)
                                    |> Maybe.withDefault 0
                            )
                , opponentCards = deck |> List.drop Config.cardsPerHand |> List.take Config.cardsPerHand
                , playedCards = []
                , probabilities = probabilities
                }
            )


removeYourCard : Card -> Game -> Game
removeYourCard card game =
    { game
        | yourCards =
            game.yourCards
                |> List.filter ((/=) card)
    }


removeOpponentCard : Card -> Game -> Game
removeOpponentCard card game =
    { game
        | opponentCards =
            game.opponentCards
                |> List.filter ((/=) card)
    }


playCard : Card -> Game -> Game
playCard card game =
    { game
        | playedCards = card :: game.playedCards
    }


isWon : Game -> Bool
isWon game =
    game.playedCards
        |> List.head
        |> Maybe.map
            (\{ goal } ->
                game.yourCards
                    ++ game.opponentCards
                    ++ game.playedCards
                    |> List.foldl
                        (\card ->
                            Dict.update (Suit.icon card.suit)
                                (\maybe ->
                                    maybe
                                        |> Maybe.withDefault 0
                                        |> (+) 1
                                        |> Just
                                )
                        )
                        Dict.empty
                    |> Goal.goalMet goal
            )
        |> Maybe.withDefault False


currentPercentage : Game -> Int
currentPercentage game =
    game.playedCards
        |> List.head
        |> Maybe.andThen
            (\card ->
                game.probabilities
                    |> Dict.get (Goal.description card.goal)
            )
        |> Maybe.withDefault 100
