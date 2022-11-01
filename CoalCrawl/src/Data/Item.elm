module Data.Item exposing (..)


type Item
    = Coal
    | IronOre


toString : Item -> String
toString item =
    case item of
        Coal ->
            "Coal"

        IronOre ->
            "Iron Ore"
