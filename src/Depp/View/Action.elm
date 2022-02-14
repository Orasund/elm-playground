module Depp.View.Action exposing (view)

import Cards exposing (Face(..), Suit(..))
import Depp.Data.Deck as Deck
import Depp.Data.Game exposing (Action(..), Game)
import Depp.View.Card as Card


view : Game -> (Action -> msg) -> Action -> { label : String, onClick : Maybe msg }
view game onClick action =
    { label =
        case action of
            PlayCard args ->
                "Play " ++ Card.toString game args.hand ++ " onto " ++ Card.toString game args.board

            SwapCards args ->
                "Swap " ++ Card.toString game args.hand ++ " with " ++ Card.toString game args.board

            Redraw card ->
                "Keep " ++ Card.toString game card ++ " and Redraw"
    , onClick = action |> onClick |> Just
    }
