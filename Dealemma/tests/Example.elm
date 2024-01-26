module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game exposing (Card)
import Goal exposing (Goal)
import List.Extra
import Random
import Random.List
import Suit exposing (Suit)
import Test exposing (..)


type alias Random a =
    Random.Generator a


randomDeck : Random (List Suit)
randomDeck =
    Suit.asList
        |> List.concatMap (List.repeat 4)
        |> Random.List.shuffle
        |> Random.map (List.take 8)


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


suite : Test
suite =
    let
        ( decks, _ ) =
            randomDeck
                |> Random.list 100
                |> (\random -> Random.step random (Random.initialSeed 42))
    in
    Goal.asList
        |> List.map
            (\card ->
                Test.test
                    ("Simulate " ++ Goal.goalDescription card)
                    (\() ->
                        decks
                            |> List.map (simulateGame card)
                            |> List.Extra.count identity
                            |> Expect.equal (Goal.probability card)
                    )
            )
        |> Test.describe "Check probability"
