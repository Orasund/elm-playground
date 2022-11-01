module Data.Item exposing (..)


type Item
    = Coal
    | Iron
    | Gold


toString : Item -> String
toString item =
    case item of
        Coal ->
            "Coal"

        Iron ->
            "Iron"

        Gold ->
            "Gold"
