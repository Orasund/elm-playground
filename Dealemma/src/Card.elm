module Card exposing (..)

import Config
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


buildCard : Suit -> Goal -> Card
buildCard value goal =
    { suit = value, goal = goal }


specialCards : List Card
specialCards =
    Suit.asList
        |> List.concatMap
            (\suit ->
                Goal.special
                    |> List.map (buildCard suit)
            )


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
