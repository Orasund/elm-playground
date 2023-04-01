module Main exposing (..)

import Fish
import FishPattern
import Html exposing (Html)
import Random
import View


main : Html msg
main =
    let
        seed =
            Random.initialSeed 40

        generators =
            [ []
            , FishPattern.horizontal
            , FishPattern.vertical
            , FishPattern.diagonal1
            , FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.vertical
            , FishPattern.horizontal ++ FishPattern.diagonal1
            , FishPattern.horizontal ++ FishPattern.diagonal2
            , FishPattern.vertical ++ FishPattern.diagonal1
            , FishPattern.vertical ++ FishPattern.diagonal2
            , FishPattern.diagonal1 ++ FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.vertical ++ FishPattern.diagonal1
            , FishPattern.horizontal ++ FishPattern.vertical ++ FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.diagonal1 ++ FishPattern.diagonal2
            , FishPattern.vertical ++ FishPattern.diagonal1 ++ FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.vertical ++ FishPattern.diagonal1 ++ FishPattern.diagonal2
            ]

        ( patterns, _ ) =
            generators
                |> List.foldl
                    (\rules ->
                        Random.andThen
                            (\l ->
                                Fish.generatePattern rules
                                    |> Random.map (\p -> p :: l)
                            )
                    )
                    (Random.constant [])
                |> (\gen -> Random.step gen seed)
    in
    patterns
        |> List.reverse
        |> List.map View.fish
        |> Html.div []
