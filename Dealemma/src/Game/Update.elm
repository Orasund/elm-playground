module Game.Update exposing (..)

import Game exposing (Card, Game, currentPercentage)
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
                Goal.probability card.goal <= currentPercentage
            )
        |> List.sortBy (\card -> 100 - Goal.probability card.goal)
        |> List.head
        |> Maybe.map
            (\card ->
                Random.float 0 100
                    |> Random.map
                        (\float ->
                            if float < toFloat (Goal.probability card.goal) || currentPercentage == 100 then
                                game
                                    |> Game.removeOpponentCard card
                                    |> Game.playCard card
                                    |> Just

                            else
                                Nothing
                        )
            )
        |> Maybe.withDefault (Random.constant Nothing)
