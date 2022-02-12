module Depp.View.Action exposing (view)

import Cards exposing (Face(..), Suit(..))
import Depp.Data.Deck as Deck
import Depp.Data.Game exposing (Action(..))
import Depp.View.Card as Card


view : (Action -> msg) -> Action -> { label : String, onClick : Maybe msg }
view onClick action =
    { label =
        case action of
            PlayCard args ->
                "Play " ++ Card.toString args.hand ++ " onto " ++ Card.toString args.board

            Redraw card ->
                "Redraw and keep " ++ Card.toString card
    , onClick = action |> onClick |> Just
    }
