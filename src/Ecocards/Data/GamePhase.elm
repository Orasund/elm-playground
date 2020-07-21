module Ecocards.Data.GamePhase exposing (GamePhase(..), Model)

import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.Move as Move exposing (Move)
import Set exposing (Set)


type GamePhase
    = WaitingForOpponent
    | Thinking { played : Set Int }
    | Tapping Move
    | Won
    | Lost


endPhase : ( GamePhase, Game ) -> ( GamePhase, Game )
endPhase ( gamePhase, game ) =
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

        Won ->
            ( Won, game )

        Lost ->
            ( Lost, game )



--------------------------------------------------------------------------------


type alias Model =
    { game : Game
    , phase : GamePhase
    }
