module Example exposing (..)

import Card exposing (Card)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra
import Random
import Random.List
import Test exposing (..)


type alias Random a =
    Random.Generator a


simulateGame : Card -> Random Bool
simulateGame card =
    List.range 1 4
        |> List.concatMap (List.repeat 4)
        |> Random.List.shuffle
        |> Random.map (List.take 8)
        |> Random.map
            (\list ->
                list
                    |> List.Extra.gatherEquals
                    |> List.map
                        (Tuple.mapSecond
                            (\l ->
                                List.length l + 1
                            )
                        )
                    |> Dict.fromList
            )
        |> Random.map (Card.goalMet card)


suite : Test
suite =
    Card.asList
        |> List.map
            (\card ->
                Test.test
                    ("Simulate " ++ Card.goalDescription card)
                    (\() ->
                        simulateGame card
                            |> Random.list 1000
                            |> Random.map (List.Extra.count identity)
                            |> (\random -> Random.step random (Random.initialSeed 42))
                            |> Tuple.first
                            |> Expect.equal (Card.probability card)
                    )
            )
        |> Test.describe "Check probability"
