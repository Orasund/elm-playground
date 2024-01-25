module View.Color exposing (..)

import Color exposing (Color(..))


toString : Color -> String
toString color =
    case color of
        Red ->
            "red"

        Blue ->
            "blue"
