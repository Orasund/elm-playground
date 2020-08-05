module Ecocards.Data.GamePhase exposing (GamePhase(..), autoPlay, autoTap, emptyMove, end, play, tap)

import Array
import Bag
import Dict exposing (Dict)
import Dict.Extra as Dict
import Ecocards.Data.Animal as Animal exposing (Animal, Behaviour(..))
import Ecocards.Data.Bag as Bag
import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.Move as Move exposing (Move)
import List.Extra as List
import Set exposing (Set)


type GamePhase
    = WaitingForOpponent
    | Thinking { played : Maybe (Set Int) }
    | Tapping Move
    | Finished Bool


end :
    { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
end { gamePhase, game } =
    case gamePhase of
        WaitingForOpponent ->
            Ok { gamePhase = Thinking { played = Nothing }, game = game }

        Thinking { played } ->
            if played == Nothing then
                Err "Nothing played yet"

            else
                let
                    newGame =
                        game |> Game.endTurn
                in
                case newGame |> Game.isFinished of
                    Just bool ->
                        Ok { gamePhase = Finished bool, game = newGame }

                    Nothing ->
                        Ok { gamePhase = WaitingForOpponent, game = newGame }

        Tapping move ->
            Game.tapAnimal move game
                |> Result.map
                    (\g ->
                        { gamePhase =
                            Thinking
                                { played =
                                    move.played
                                        |> Set.insert move.animalId
                                        |> Just
                                }
                        , game = g
                        }
                    )

        Finished bool ->
            Ok { gamePhase = Finished bool, game = game }


play :
    { index : Int }
    -> { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
play { index } { gamePhase, game } =
    case gamePhase of
        Thinking { played } ->
            game
                |> Game.play { index = index }
                |> Result.map
                    (\g ->
                        { gamePhase =
                            Thinking
                                { played =
                                    played
                                        |> Maybe.withDefault Set.empty
                                        |> Just
                                }
                        , game = g
                        }
                    )

        _ ->
            Ok { gamePhase = gamePhase, game = game }


autoPlay :
    { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
autoPlay { gamePhase, game } =
    if game.yourArea.hand |> Array.isEmpty then
        --all cards are played
        game.yourArea.placed
            |> Dict.toList
            |> List.sortBy
                (\( id, _ ) ->
                    game.animals
                        |> Dict.get id
                        |> Maybe.map .strength
                        |> Maybe.withDefault 0
                )
            |> List.filterMap
                (\( id, { isTapped } ) ->
                    if isTapped then
                        Nothing

                    else
                        case
                            { gamePhase = gamePhase, game = game }
                                |> autoTap { id = id }
                        of
                            Ok ok ->
                                Just <| Ok ok

                            Err _ ->
                                Nothing
                )
            |> List.head
            |> Maybe.withDefault ({ gamePhase = gamePhase, game = game } |> end)

    else
        { gamePhase = gamePhase, game = game }
            |> play { index = 0 }


tap :
    Move
    -> { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
tap move { gamePhase, game } =
    case gamePhase of
        Thinking { played } ->
            if (played |> Maybe.withDefault Set.empty) == move.played then
                Ok
                    { gamePhase = Tapping move
                    , game = game
                    }

            else
                Err "Bug: played Cards are different"

        _ ->
            Ok { gamePhase = gamePhase, game = game }


emptyMove : { id : Int, played : Set Int, game : Game } -> Maybe Move
emptyMove { id, played, game } =
    if game.yourArea.placed |> Dict.member id then
        Just
            { animalId = id
            , selected = Set.empty
            , played = played
            }

    else
        Nothing


calcMove : { id : Int, played : Set Int, game : Game } -> Maybe Move
calcMove { id, played, game } =
    case game.animals |> Dict.get id of
        Just tappingAnimal ->
            let
                selected : Set Int
                selected =
                    let
                        amounts =
                            tappingAnimal |> Animal.getAmounts

                        toAnimals : Dict Int a -> Dict Int Animal
                        toAnimals =
                            Dict.filterMap
                                (\i _ ->
                                    game.animals
                                        |> Dict.get i
                                        |> Maybe.andThen
                                            (\animal ->
                                                if
                                                    animal.strength
                                                        < tappingAnimal.strength
                                                        && (case tappingAnimal.behaviour of
                                                                Predator _ ->
                                                                    animal.biome == tappingAnimal.biome

                                                                _ ->
                                                                    True
                                                           )
                                                then
                                                    Just animal

                                                else
                                                    Nothing
                                            )
                                )

                        yourAnimalList =
                            game.yourArea.placed
                                |> Dict.remove id
                                |> toAnimals

                        oppAnimalList =
                            game.oppArea.placed
                                |> toAnimals

                        toBag =
                            Dict.toList
                                >> List.map (\( _, { strength } ) -> strength)
                                >> List.group
                                >> List.map (Tuple.mapSecond (List.length >> (+) 1))
                                >> Bag.fromList
                    in
                    case tappingAnimal.behaviour of
                        Herbivores _ ->
                            Set.empty

                        _ ->
                            Bag.findMinMaxSubset amounts
                                { maxBag =
                                    oppAnimalList
                                        |> toBag
                                , minBag =
                                    yourAnimalList
                                        |> toBag
                                }
                                |> Maybe.map
                                    (\{ maxBag, minBag } ->
                                        Set.union
                                            (yourAnimalList |> Move.getSubset minBag)
                                            (oppAnimalList |> Move.getSubset maxBag)
                                    )
                                |> Maybe.withDefault Set.empty
            in
            emptyMove { id = id, played = played, game = game }
                |> Maybe.map
                    (\move ->
                        { move
                            | selected =
                                selected
                        }
                    )

        Nothing ->
            Nothing


autoTap :
    { id : Int }
    -> { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
autoTap { id } { gamePhase, game } =
    case gamePhase of
        Thinking { played } ->
            calcMove
                { id = id
                , played = played |> Maybe.withDefault Set.empty
                , game = game
                }
                |> Maybe.map
                    (\move ->
                        { gamePhase = Thinking { played = played }
                        , game = game
                        }
                            |> tap move
                            |> Result.andThen end
                    )
                |> Maybe.withDefault (Err "Bug: Animal Id not found")

        _ ->
            Ok { gamePhase = gamePhase, game = game }
