module Game.Update exposing (..)

import Dict
import Game exposing (CardId, Game)
import Game.Evaluate exposing (probabilities)
import Goal
import Random exposing (Generator)
import Set


type alias Random a =
    Generator a


playCard : CardId -> Game -> Game
playCard =
    Game.playCard


challengeGoal : Game -> Bool
challengeGoal game =
    Game.isWon game


opponentsTurn : Game -> Random (Maybe Game)
opponentsTurn game =
    let
        probabilities =
            (game.opponentCards |> Set.toList)
                ++ (game.playedCards |> List.take 1)
                |> List.filterMap (Game.getCardFrom game)
                |> Game.Evaluate.probabilities
                    { deck =
                        Set.toList game.outOfPlay
                            ++ Set.toList game.yourCards
                            |> List.filterMap (Game.getCardFrom game)
                            |> List.map .suit
                    , open =
                        Set.toList game.opponentCards
                            ++ game.playedCards
                            |> List.filterMap (Game.getCardFrom game)
                            |> List.map .suit
                    }
    in
    if
        game.playedCards
            |> List.head
            |> Maybe.andThen (Game.getCardFrom game)
            |> Maybe.andThen (\card -> Dict.get (Goal.description card.goal) probabilities)
            |> Maybe.map (\currentProbability -> currentProbability > 0)
            |> Maybe.withDefault True
    then
        game.opponentCards
            |> Set.toList
            |> List.filterMap
                (\cardId ->
                    cardId
                        |> Game.getCardFrom game
                        |> Maybe.map (Tuple.pair cardId)
                )
            |> List.filter
                (\( _, card ) ->
                    (game.values
                        |> Dict.get
                            (card.goal
                                |> Goal.description
                            )
                        |> Maybe.withDefault 0
                    )
                        >= Game.currentValue game
                )
            |> List.sortBy
                (\( _, card ) ->
                    100
                        - (probabilities
                            |> Dict.get (Goal.description card.goal)
                            |> Maybe.withDefault 0
                          )
                )
            |> List.head
            |> Maybe.map
                (\( cardId, card ) ->
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
                                        || Game.currentValue game
                                        == 0
                                then
                                    game
                                        |> Game.playCard cardId
                                        |> Just

                                else
                                    Nothing
                            )
                )
            |> Maybe.withDefault (Random.constant Nothing)

    else
        Random.constant Nothing
