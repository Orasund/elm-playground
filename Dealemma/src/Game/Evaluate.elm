module Game.Evaluate exposing (..)

import Card exposing (Card)
import Config
import Dict exposing (Dict)
import Goal exposing (Goal)
import List.Extra
import Random exposing (Generator)
import Random.List
import Suit exposing (Suit)


type alias Random a =
    Generator a


probabilities : { deck : List Suit, open : List Suit } -> List Card -> Dict String Int
probabilities args cards =
    let
        randomDeck : Random (List Suit)
        randomDeck =
            args.deck
                |> Random.List.shuffle
                |> Random.map (List.take (Config.cardsPerHand * 2))

        simulateGame : Goal -> List Suit -> Bool
        simulateGame card list =
            list
                |> List.Extra.gatherEquals
                |> List.map
                    (\( suit, l ) ->
                        ( Suit.icon suit, List.length l + 1 )
                    )
                |> Dict.fromList
                |> Goal.goalMet card

        ( decks, _ ) =
            randomDeck
                |> Random.list 100
                |> (\random -> Random.step random (Random.initialSeed 42))
    in
    cards
        |> List.map
            (\card ->
                ( Goal.description card.goal
                , decks
                    |> List.map (simulateGame card.goal)
                    |> List.Extra.count identity
                )
            )
        |> Dict.fromList
