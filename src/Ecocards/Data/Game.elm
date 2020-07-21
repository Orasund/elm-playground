module Ecocards.Data.Game exposing (Game)

import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Ecocards.Data.Animal exposing (Animal, Biome)
import Ecocards.Data.GameArea as GameArea exposing (GameArea)
import Set exposing (Set)


type alias Move =
    { card : Int
    , selected : Set Int
    , played : Set Int
    , maxAmount : Int
    , minAmount : Int
    }



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


tapHerbivores : { amount : Int } -> Move -> Game -> Game
tapHerbivores { amount } move game =
    if amount <= (move.played |> Set.size) then
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
        game


tapOmnivorous : { minAmount : Int, maxAmount : Int } -> Move -> Game -> Game
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
        game


tabPredator :
    { minAmount : Int
    , maxAmount : Int
    , biome : Biome
    }
    -> Move
    -> Game
    -> Game
tabPredator { minAmount, maxAmount, biome } move game =
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
        game



--------------------------------------------------------------------------------


type GamePhase
    = WaitingForOpponent
    | Thinking { played : Set Int }
    | Tapping Move
    | Won
    | Lost


type alias Model =
    { game : Game
    , phase : GamePhase
    }
