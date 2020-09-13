module LinesCards.Card exposing (Card(..), Color(..), dashes)

import LinesCards.View as View


type Color
    = Y
    | G
    | B
    | R


type Card
    = Cross Color Color
    | VEdges Color Color
    | HEdges Color Color


dashes : Color -> String
dashes color =
    case color of
        Y ->
            (String.fromFloat <| View.relative <| 0)
                ++ " "
                ++ (String.fromFloat <| View.relative <| 2)

        G ->
            ""

        B ->
            (String.fromFloat <| View.relative <| 2)
                ++ " "
                ++ (String.fromFloat <| View.relative <| 2)

        R ->
            (String.fromFloat <| View.relative <| 0)
                ++ " "
                ++ (String.fromFloat <| View.relative <| 2)
                ++ " "
                ++ (String.fromFloat <| View.relative <| 0)
                ++ " "
                ++ (String.fromFloat <| View.relative <| 2)
                ++ " "
                ++ (String.fromFloat <| View.relative <| 2)
                ++ " "
                ++ (String.fromFloat <| View.relative <| 2)
