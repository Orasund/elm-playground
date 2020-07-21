module Ecocards.Data.GamePhase exposing (GamePhase(..), end)

import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.Move as Move exposing (Move)
import Set exposing (Set)


type GamePhase
    = WaitingForOpponent
    | Thinking { played : Set Int }
    | Tapping Move
    | Finished Bool


end : ( GamePhase, Game ) -> ( GamePhase, Game )
end ( gamePhase, game ) =
    case gamePhase of
        WaitingForOpponent ->
            ( Thinking { played = Set.empty }, game )

        Thinking _ ->
            ( WaitingForOpponent, game |> Game.endTurn )

        Tapping move ->
            ( Thinking { played = move.played }
            , Game.tapAnimal move.card move game
                |> Result.withDefault game
            )

        Finished bool ->
            ( Finished bool, game )
