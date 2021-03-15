module Mezzo.View.Suit exposing (toColor)

import Color exposing (Color)
import Mezzo.Data.Card exposing (Suit)


toColor : Suit -> Color
toColor suit =
    case ( suit.red, suit.blue, suit.yellow ) of
        ( False, False, True ) ->
            Color.rgb255 212 212 106

        --yellow
        ( False, True, False ) ->
            Color.rgb255 79 97 143

        --blue
        ( False, True, True ) ->
            Color.rgb255 85 170 85

        --green
        ( True, False, False ) ->
            Color.rgb255 212 106 106

        --red
        ( True, False, True ) ->
            Color.rgb255 212 178 106

        --orange
        ( True, True, False ) ->
            Color.rgb255 117 75 142

        --violette
        ( True, True, True ) ->
            Color.rgb255 0 0 0

        --black
        ( False, False, False ) ->
            Color.rgb255 255 255 255



--white
