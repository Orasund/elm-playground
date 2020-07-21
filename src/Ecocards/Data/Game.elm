module Ecocards.Data.Game exposing (Game, tapAnimal)

import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Ecocards.Data.Animal exposing (Animal, Behaviour(..), Biome)
import Ecocards.Data.GameArea as GameArea exposing (GameArea)
import Ecocards.Data.Move exposing (Move)
import Set exposing (Set)



--------


type alias Game =
    { yourArea : GameArea
    , oppArea : GameArea
    , animals : Dict Int Animal
    , nextId : Int
    }


play : { index : Int } -> Game -> Game
play { index } ({ yourArea } as game) =
    yourArea.hand
        |> Array.get index
        |> Maybe.map
            (\animal ->
                { game
                    | yourArea =
                        { yourArea
                            | hand = yourArea.hand |> Array.removeAt index
                            , placed =
                                yourArea.placed
                                    |> Dict.insert game.nextId { isTapped = False }
                        }
                    , animals = game.animals |> Dict.insert game.nextId animal
                    , nextId = game.nextId + 1
                }
            )
        |> Maybe.withDefault game


remove : { id : Int } -> Game -> Game
remove { id } ({ yourArea } as game) =
    { game
        | yourArea = game.yourArea |> GameArea.remove id
        , oppArea = game.oppArea |> GameArea.remove id
        , animals = game.animals |> Dict.remove id
    }


tapHerbivores : { amount : Int } -> Move -> Game -> Result () Game
tapHerbivores { amount } move game =
    if amount <= (move.played |> Set.size) then
        Ok
            { game
                | yourArea =
                    game.yourArea
                        |> GameArea.tap move.card
                        |> (game.animals
                                |> Dict.get move.card
                                |> Maybe.map GameArea.add
                                |> Maybe.withDefault identity
                           )
            }

    else
        Err ()


tapOmnivorous : { minAmount : Int, maxAmount : Int } -> Move -> Game -> Result () Game
tapOmnivorous { minAmount, maxAmount } move game =
    let
        amount =
            move.selected
                |> Set.foldl
                    (\id ->
                        game.animals
                            |> Dict.get id
                            |> Maybe.map .strength
                            |> Maybe.withDefault 0
                            |> (+)
                    )
                    0
    in
    if (minAmount <= amount) && (amount <= maxAmount) then
        Ok
            { game
                | yourArea =
                    game.yourArea
                        |> GameArea.tap move.card
                        |> GameArea.removeSet move.selected
                        |> (if minAmount == amount then
                                game.animals
                                    |> Dict.get move.card
                                    |> Maybe.map GameArea.add
                                    |> Maybe.withDefault identity

                            else
                                identity
                           )
            }

    else
        Err ()


tapPredator :
    { minAmount : Int
    , maxAmount : Int
    , biome : Biome
    }
    -> Move
    -> Game
    -> Result () Game
tapPredator { minAmount, maxAmount, biome } move game =
    if
        move.selected
            |> Set.toList
            |> List.all
                (\id ->
                    game.animals |> Dict.get id |> Maybe.map (.biome >> (==) biome) |> Maybe.withDefault True
                )
    then
        tapOmnivorous { minAmount = minAmount, maxAmount = maxAmount } move game

    else
        Err ()


tapAnimal : Int -> Move -> Game -> Result () Game
tapAnimal id move game =
    game.animals
        |> Dict.get id
        |> Maybe.map
            (\{ behaviour } ->
                (case behaviour of
                    Predator biome ( minAmount, maxAmount ) ->
                        tapPredator
                            { minAmount = minAmount
                            , maxAmount = maxAmount
                            , biome = biome
                            }

                    Herbivores amount ->
                        tapHerbivores { amount = amount }

                    Omnivorous ( minAmount, maxAmount ) ->
                        tapOmnivorous
                            { minAmount = minAmount
                            , maxAmount = maxAmount
                            }
                )
                    move
                    game
            )
        |> Maybe.withDefault (Err ())


endTurn : Game -> Game
endTurn game =
    { game
        | yourArea = game.yourArea |> GameArea.endTurn
        , animals =
            game.yourArea.placed
                |> Dict.filter (\_ { isTapped } -> not isTapped)
                |> Dict.keys
                |> List.foldl Dict.remove game.animals
    }
