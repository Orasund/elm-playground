module Game.Update exposing (..)

import Card exposing (Card)
import Dict
import Game exposing (Game, currentPercentage)
import Game.Evaluate exposing (probabilities)
import Goal
import Random exposing (Generator)


type alias Random a =
    Generator a


playCard : Card -> Game -> Game
playCard card game =
    game
        |> Game.removeYourCard card
        |> Game.playCard card


challengeGoal : Game -> Bool
challengeGoal game =
    Game.isWon game


opponentsTurn : Game -> Random (Maybe Game)
opponentsTurn game =
    let
        probabilities =
            game.opponentCards
                ++ (game.playedCards |> List.take 1)
                |> Game.Evaluate.probabilities
                    { deck = game.outOfPlay ++ game.yourCards |> List.map .suit
                    , open = game.opponentCards ++ game.playedCards |> List.map .suit
                    }
    in
    if
        game.playedCards
            |> List.head
            |> Maybe.andThen (\card -> Dict.get (Goal.description card.goal) probabilities)
            |> Maybe.map (\currentProbability -> currentProbability > 0)
            |> Maybe.withDefault True
    then
        game.opponentCards
            |> List.filter
                (\card ->
                    (game.probabilities
                        |> Dict.get (Goal.description card.goal)
                        |> Maybe.withDefault 0
                    )
                        <= Game.currentPercentage game
                )
            |> List.sortBy
                (\card ->
                    100
                        - (probabilities
                            |> Dict.get (Goal.description card.goal)
                            |> Maybe.withDefault 0
                          )
                )
            |> List.head
            |> Maybe.map
                (\card ->
                    Random.float 0 100
                        |> Random.map
                            (\float ->
                                if
                                    float
                                        < toFloat
                                            (probabilities
                                                |> Dict.get (Goal.description card.goal)
                                                |> Maybe.withDefault 0
                                            )
                                        || Game.currentPercentage game
                                        == 100
                                then
                                    game
                                        |> Game.removeOpponentCard card
                                        |> Game.playCard card
                                        |> Just

                                else
                                    Nothing
                            )
                )
            |> Maybe.withDefault (Random.constant Nothing)

    else
        Random.constant Nothing
