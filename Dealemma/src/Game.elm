module Game exposing (..)

import Config
import Dict
import Goal exposing (Goal)
import Random exposing (Generator)
import Random.List
import Suit exposing (Suit)


type alias Random a =
    Generator a


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


newDeck : List Goal -> Random (List Card)
newDeck list =
    Suit.asList
        |> List.concatMap (List.repeat Config.cardsPerSuit)
        |> Random.List.shuffle
        |> Random.map
            (\randomList ->
                List.map2 buildCard
                    randomList
                    list
            )


fromGoals : List Goal -> Random Game
fromGoals list =
    newDeck list
        |> Random.andThen fromDeck


fromDeck : List Card -> Random Game
fromDeck sortedDeck =
    Random.List.shuffle sortedDeck
        |> Random.map
            (\deck ->
                { yourCards = List.take Config.cardsPerHand deck |> List.sortBy (\card -> Goal.probability card.goal)
                , opponentCards = deck |> List.drop Config.cardsPerHand |> List.take Config.cardsPerHand
                , playedCards = []
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
        |> Maybe.map
            (\card ->
                Goal.probability card.goal
            )
        |> Maybe.withDefault 100
