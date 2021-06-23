module GreenFields.Data.Resource exposing (Resource(..), fromString, toString)


type Resource
    = Wood
    | Stone
    | Sheep
    | Silver


toString : Resource -> String
toString resource =
    case resource of
        Wood ->
            "Wood"

        Stone ->
            "Stone"

        Sheep ->
            "Sheep"

        Silver ->
            "Silver"


fromString : String -> Resource
fromString string =
    case string of
        "Wood" ->
            Wood

        "Stone" ->
            Stone

        "Sheep" ->
            Sheep

        "Silver" ->
            Silver

        _ ->
            Silver
