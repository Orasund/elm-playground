module Tile exposing (Tile(..), toString)


type Tile
    = Stone
    | Leaf
    | SpiderWeb


toString : Tile -> String
toString tile =
    case tile of
        Stone ->
            "\u{1FAA8}"

        Leaf ->
            "🍂"

        SpiderWeb ->
            "🕸️"
