module Data.Item exposing (..)


type Item
    = Coal
    | Iron
    | Gold


toString : Item -> String
toString item =
    case item of
        Coal ->
            "\u{1FAA8} Coal"

        Iron ->
            "🔩 Iron"

        Gold ->
            "\u{1FA99} Gold"
