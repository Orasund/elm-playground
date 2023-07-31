module Level exposing (..)


type Level
    = Level1
    | Level2


previous : Level -> Level
previous level =
    case level of
        Level1 ->
            Level1

        Level2 ->
            Level2


next : Level -> Maybe Level
next level =
    case level of
        Level1 ->
            Level2 |> Just

        Level2 ->
            Nothing


toString : Level -> String
toString level =
    case level of
        Level1 ->
            "1"

        Level2 ->
            "2"
