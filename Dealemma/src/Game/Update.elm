module Game.Update exposing (..)

import Game exposing (Card, Game, currentPercentage)
import Goal


playCard : Card -> Game -> Game
playCard card game =
    game
        |> Game.removeYourCard card
        |> Game.playCard card


challengeGoal : Game -> Bool
challengeGoal game =
    Game.isWon game


opponentsTurn : Game -> Maybe Game
opponentsTurn game =
    let
        currentPercentage =
            Game.currentPercentage game
    in
    game.opponentCards
        |> List.filter
            (\card ->
                Goal.probability card.goal <= currentPercentage
            )
        |> List.sortBy (\card -> 100 - Goal.probability card.goal)
        |> List.head
        |> Maybe.map
            (\card ->
                game
                    |> Game.removeOpponentCard card
                    |> Game.playCard card
            )
