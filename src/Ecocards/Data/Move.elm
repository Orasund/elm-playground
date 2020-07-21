module Ecocards.Data.Move exposing (Move)

import Set exposing (Set)


type alias Move =
    { card : Int
    , selected : Set Int
    , played : Set Int
    , maxAmount : Int
    , minAmount : Int
    }
