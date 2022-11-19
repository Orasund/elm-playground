module Data.Item exposing (..)


type Item
    = Coal
    | Iron
    | Gold


toString : Item -> String
toString item =
    (item
            |> toChar
            |> String.fromChar )
           ++ (
    case item of
        Coal ->
            "Coal"

        Iron ->
            "Iron"

        Gold ->
            "Gold")

toChar : Item -> Char
toChar item =
    case item of
        Coal ->
            '⚫'

        Iron ->
            '🔘'

        Gold ->
            '🟡'