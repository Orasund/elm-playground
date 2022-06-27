module Tile exposing (Tile(..), toString)


type Tile
    = Stone
    | Bug { visible : Bool }
    | Leaf


toString : Tile -> String
toString tile =
    case tile of
        Stone ->
            "\u{1FAA8}"

        Bug _ ->
            "\u{1FAB2}"

        Leaf ->
            "🍂"
