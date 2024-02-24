module Object exposing (Object(..), toString)


type Object
    = Stone
    | Leaf
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
