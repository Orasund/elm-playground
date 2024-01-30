module Game.Update exposing (..)

import Card exposing (Card)
import Dict
import Game exposing (Game, currentPercentage)
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
        currentPercentage =
            Game.currentPercentage game
    in
    game.opponentCards
        |> List.filter
            (\card ->
                (game.probabilities
                    |> Dict.get (Goal.description card.goal)
                    |> Maybe.withDefault 0
                )
                    <= currentPercentage
            )
        |> List.sortBy
            (\card ->
                100
                    - (game.probabilities
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
                                        (game.probabilities
                                            |> Dict.get (Goal.description card.goal)
                                            |> Maybe.withDefault 0
                                        )
                                    || currentPercentage
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
