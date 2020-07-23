module Ecocards.Data.Game exposing (Game, isFinished, endTurn, play, swapAreas, tapAnimal)

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


play : { index : Int } -> Game -> Result String Game
play { index } ({ yourArea } as game) =
    yourArea.hand
        |> Array.get index
        |> Maybe.map
            (\animal ->
                Ok
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
        |> Maybe.withDefault
            (Err
                ("index out of bounds:"
                    ++ String.fromInt index
                    ++ " of "
                    ++ String.fromInt (yourArea.hand |> Array.length)
                )
            )


remove : { id : Int } -> Game -> Game
remove { id } ({ yourArea } as game) =
    { game
        | yourArea = game.yourArea |> GameArea.remove id
        , oppArea = game.oppArea |> GameArea.remove id
        , animals = game.animals |> Dict.remove id
    }


tapHerbivores : { amount : Int } -> Move -> Game -> Result String Game
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
        Err
            ("Wrong amount: "
                ++ String.fromInt amount
                ++ ", expected "
                ++ String.fromInt (move.played |> Set.size)
                ++ " or less"
            )


tapOmnivorous : { minAmount : Int, maxAmount : Int } -> Move -> Game -> Result String Game
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
                        |> (if maxAmount == amount then
                                game.animals
                                    |> Dict.get move.card
                                    |> Maybe.map GameArea.add
                                    |> Maybe.withDefault identity

                            else
                                identity
                           )
                , oppArea =
                    game.oppArea
                        |> GameArea.removeSet move.selected
            }

    else
        Err
            ("wrong amount:"
                ++ String.fromInt amount
                ++ ", expected between "
                ++ String.fromInt minAmount
                ++ " and "
                ++ String.fromInt maxAmount
            )


tapPredator :
    { minAmount : Int
    , maxAmount : Int
    , biome : Biome
    }
    -> Move
    -> Game
    -> Result String Game
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
        Err "not all selected have the expected biome."


tapAnimal : Int -> Move -> Game -> Result String Game
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
        |> Maybe.withDefault (Err ("Id not found in game.animals: " ++ String.fromInt id))


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


swapAreas : Game -> Game
swapAreas game =
    { game
        | yourArea = game.oppArea
        , oppArea = game.yourArea
    }


isFinished : Game -> Maybe Bool
isFinished game =
    if (game.yourArea.deck |> List.length) >= 6 then
        Just True

    else if (game.yourArea.hand |> Array.length) < 3 then
        Just False

    else
        Nothing
