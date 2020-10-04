module Sprawlopolis.Color exposing (Color(..), toString)


type Color
    = Y
    | G
    | B
    | R


toString : Color -> String
toString color =
    case color of
        Y ->
            yellow

        G ->
            green

        B ->
            blue

        R ->
            red


yellow : String
yellow =
    "#F0E442"


green : String
green =
    "#009E73"


blue : String
blue =
    "#56B4E9"


red : String
red =
    "#D55E00"
