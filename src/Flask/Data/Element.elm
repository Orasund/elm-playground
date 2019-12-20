module Flask.Data.Element exposing (Element(..), toString)


type Element
    = Red
    | Blue
    | Yellow
    | Green
    | Any


toString : Element -> String
toString element =
    case element of
        Red ->
            "💥"

        Blue ->
            "📘"

        Yellow ->
            "💰"

        Green ->
            "💚"

        Any ->
            "❔"
