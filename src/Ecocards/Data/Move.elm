module Ecocards.Data.Move exposing (Move, toggle)

import Set exposing (Set)
import Set.Extra as Set


type alias Move =
    { card : Int
    , selected : Set Int
    , played : Set Int
    , maxAmount : Int
    , minAmount : Int
    }


toggle : Int -> Move -> Move
toggle id move =
    { move
        | selected = move.selected |> Set.toggle id
    }
