module Bomb16.View.Color exposing (black, fromInt, red)

import Color exposing (Color)


black : Color
black =
    Color.rgb255 117 115 150


red : Color
red =
    Color.rgb255 216 140 154


fromInt : Int -> Color
fromInt n =
    Color.hsl 0.608
        (0.1 * logBase 2 (toFloat n))
        (0.9 - 0.02 * logBase 2 (toFloat n))
