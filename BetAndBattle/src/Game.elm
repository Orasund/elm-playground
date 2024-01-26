module Game exposing (..)

import Config
import Dict
import Goal exposing (Goal)
import Random exposing (Generator)
import Random.List
import Suit exposing (Suit)


type alias Card =
    { suit : Suit
    , goal : Goal
    }


type alias Game =
    { yourCards : List Card
    , opponentCards : List Card
    , playedCards : List Card
    }


buildCard : Suit -> Goal -> Card
buildCard value goal =
    { suit = value, goal = goal }


fromGoals : List Goal -> Generator Game
fromGoals list =
    Suit.asList
        |> List.concatMap (List.repeat Config.cardsPerSuit)
        |> Random.List.shuffle
        |> Random.map
            (\randomList ->
                List.map2 buildCard
                    randomList
                    list
            )
        |> Random.andThen Random.List.shuffle
        |> Random.map fromDeck


fromDeck : List Card -> Game
fromDeck deck =
    let
        amount =
            Config.cardsPerHand
    in
    { yourCards = List.take amount deck
    , opponentCards = deck |> List.drop amount |> List.take amount
    , playedCards = []
    }


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
        |> Maybe.map
            (\card ->
                Goal.probability card.goal
            )
        |> Maybe.withDefault 100
