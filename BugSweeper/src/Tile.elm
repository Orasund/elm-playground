module Tile exposing (Tile(..), toString)


type Tile
    = Stone
    | Leaf
    | SpiderWeb
    | Wood


toString : Tile -> String
toString tile =
    case tile of
        Stone ->
            "🪨"

        Leaf ->
            "🍂"

        Wood ->
            "🪵"

        SpiderWeb ->
            "🕸️"
