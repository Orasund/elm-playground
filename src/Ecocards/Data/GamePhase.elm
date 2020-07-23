module Ecocards.Data.GamePhase exposing (GamePhase(..), end)

import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.Move as Move exposing (Move)
import Set exposing (Set)


type GamePhase
    = WaitingForOpponent
    | Thinking { played : Set Int }
    | Tapping Move
    | Finished Bool


end : ( GamePhase, Game ) -> Result String ( GamePhase, Game )
end ( gamePhase, game ) =
    case gamePhase of
        WaitingForOpponent ->
            Ok ( Thinking { played = Set.empty }, game )

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
                        Ok ( Finished bool, newGame )

                    Nothing ->
                        Ok ( WaitingForOpponent, newGame )

        Tapping move ->
            Game.tapAnimal move.card move game
                |> Result.map (\g -> ( Thinking { played = move.played }, g ))

        Finished bool ->
            Ok ( Finished bool, game )
