module Object exposing (Object(..), toString)


type Object
    = Stone
    | Leaf
    | SpiderWeb
    | Wood


toString : Object -> String
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
