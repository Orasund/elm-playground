module Data.Item exposing (..)


type Item
    = Coal
    | IronOre


coalString : String
coalString =
    "Coal"


ironOreString =
    "Iron Ore"


toString : Item -> String
toString item =
    case item of
        Coal ->
            coalString

        IronOre ->
            ironOreString
