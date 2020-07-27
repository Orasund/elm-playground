module Ecocards.Data.GamePhase exposing (GamePhase(..), emptyMove, end, play, tap)

import Dict exposing (Dict)
import Ecocards.Data.Animal as Animal exposing (Behaviour(..))
import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.Move as Move exposing (Move)
import Set exposing (Set)


type GamePhase
    = WaitingForOpponent
    | Thinking { played : Set Int }
    | Tapping Move
    | Finished Bool


end :
    { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
end { gamePhase, game } =
    case gamePhase of
        WaitingForOpponent ->
            Ok { gamePhase = Thinking { played = Set.empty }, game = game }

        Thinking { played } ->
            if played |> Set.isEmpty then
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
                        { gamePhase = Thinking { played = move.played }
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
                        { gamePhase = Thinking { played = played |> Set.insert (g.nextId - 1) }
                        , game = g
                        }
                    )

        _ ->
            Ok { gamePhase = gamePhase, game = game }


tap :
    Move
    -> { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
tap move { gamePhase, game } =
    case gamePhase of
        Thinking { played } ->
            if played == move.played then
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
    game.animals
        |> Dict.get id
        |> Maybe.andThen
            (\animal ->
                if game.yourArea.placed |> Dict.member id then
                    let
                        ( minAnimal, maxAnimal ) =
                            case animal.behaviour of
                                Predator _ amounts ->
                                    amounts

                                Herbivores _ ->
                                    ( 0, 0 )

                                Omnivorous amounts ->
                                    amounts
                    in
                    Just
                        { card = id
                        , selected = Set.empty
                        , played = played
                        , minAmount = minAnimal
                        , maxAmount = maxAnimal
                        }

                else
                    Nothing
            )


autoTap :
    { id : Int }
    -> { gamePhase : GamePhase, game : Game }
    -> Result String { gamePhase : GamePhase, game : Game }
autoTap { id } { gamePhase, game } =
    case gamePhase of
        Thinking { played } ->
            emptyMove { id = id, played = played, game = game }
                |> Maybe.map (\move -> { gamePhase = gamePhase, game = game } |> tap move)
                |> Maybe.withDefault (Err "Bug: Animal Id not found")

        _ ->
            Ok { gamePhase = gamePhase, game = game }
