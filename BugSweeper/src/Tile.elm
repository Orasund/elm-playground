module Tile exposing (Tile(..), toString)


type Tile
    = Stone
    | Leaf


toString : Tile -> String
toString tile =
    case tile of
        Stone ->
            "\u{1FAA8}"

        Leaf ->
            "🍂"
