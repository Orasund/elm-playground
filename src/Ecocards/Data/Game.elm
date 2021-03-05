module Ecocards.Data.Game exposing (Game, endTurn, isFinished, isValidMove, play, swapAreas, tapAnimal)

import Array
import Array.Extra as Array
import Dict exposing (Dict)
import Dict.Extra as Dict
import Ecocards.Data.Animal as Animal exposing (Animal, Biome)
import Ecocards.Data.GameArea as GameArea exposing (GameArea)
import Ecocards.Data.Move exposing (Move)
import Maybe
import Result.Extra as Result
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
remove { id } game =
    { game
        | yourArea = game.yourArea |> GameArea.remove id
        , oppArea = game.oppArea |> GameArea.remove id
        , animals = game.animals |> Dict.remove id
    }


tap :
    Animal
    -> Move
    -> Game
    -> Result String Game
tap { strength, biome, eats, symbol } move game =
    if
        move.selected
            |> Set.toList
            |> List.all
                (\id ->
                    game.animals
                        |> Dict.get id
                        |> Maybe.map
                            (\animal ->
                                eats
                                    |> Set.member (animal.biome |> Animal.biomeToString)
                            )
                        |> Maybe.withDefault True
                )
    then
        let
            ( minAmount, maxAmount ) =
                if eats |> Set.isEmpty then
                    ( 0, 0 )

                else
                    ( strength
                    , strength
                    )

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
                            |> GameArea.tap move.animalId
                            |> GameArea.removeSet move.selected
                            |> (if
                                    game.yourArea.placed
                                        |> Dict.filterMap
                                            (\id { isTapped } ->
                                                if isTapped then
                                                    game.animals
                                                        |> Dict.get id
                                                        |> Maybe.andThen
                                                            (\a ->
                                                                if a.symbol == symbol then
                                                                    Just id

                                                                else
                                                                    Nothing
                                                            )

                                                else
                                                    Nothing
                                            )
                                        |> Dict.size
                                        |> modBy 2
                                        |> (==) 1
                                then
                                    game.animals
                                        |> Dict.get move.animalId
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
                    ++ String.fromInt strength
                    ++ " and "
                    ++ String.fromInt (strength * 2)
                )

    else
        Err "Not all selected have the expected biome."


tapAnimal : Move -> Game -> Result String Game
tapAnimal move game =
    case game |> isValidMove move of
        Err err ->
            err
                |> List.head
                |> Maybe.withDefault "Move is not valid"
                |> Err

        Ok () ->
            game.animals
                |> Dict.get move.animalId
                |> Maybe.map
                    (\animal ->
                        tap
                            animal
                            move
                            game
                    )
                |> Maybe.withDefault
                    (Err
                        ("Id not found in game.animals: "
                            ++ String.fromInt move.animalId
                        )
                    )


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


isValidMove : Move -> Game -> Result (List String) ()
isValidMove move game =
    game.animals
        |> Dict.get move.animalId
        |> Maybe.map
            (\animal ->
                let
                    eats =
                        animal.eats

                    amount =
                        if eats |> Set.isEmpty then
                            0

                        else
                            animal.strength

                    isAnimalOwned =
                        if
                            game.yourArea.placed
                                |> Dict.get move.animalId
                                |> Maybe.map (\{ isTapped } -> not isTapped)
                                |> Maybe.withDefault False
                        then
                            Ok ()

                        else
                            Err "Bug: Animal is not Owned"

                    isAmountValid =
                        let
                            n =
                                move.selected
                                    |> Set.foldl
                                        (\id out ->
                                            game.animals
                                                |> Dict.get id
                                                |> Maybe.map .strength
                                                |> Maybe.withDefault 0
                                                |> (+) out
                                        )
                                        0
                        in
                        if amount == n then
                            Ok ()

                        else
                            Err <|
                                "Amount is not valid, should be "
                                    ++ String.fromInt amount
                                    ++ " but is "
                                    ++ String.fromInt n

                    isSelectedValid =
                        if
                            move.selected
                                |> Set.toList
                                |> List.all
                                    (\id ->
                                        game.animals
                                            |> Dict.get id
                                            |> Maybe.map
                                                (\{ biome } ->
                                                    eats
                                                        |> Set.member (biome |> Animal.biomeToString)
                                                )
                                            |> Maybe.withDefault False
                                    )
                        then
                            Ok ()

                        else
                            Err "Selected animals are not valid"
                in
                [ isAnimalOwned
                , isAmountValid
                , isSelectedValid
                ]
                    |> Result.partition
                    |> Tuple.second
            )
        |> Maybe.withDefault [ "Bug: Animal Id not valid" ]
        |> (\list ->
                if list |> List.isEmpty then
                    Ok ()

                else
                    Err list
           )
