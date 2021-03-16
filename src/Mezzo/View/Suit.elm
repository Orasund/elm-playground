module Mezzo.View.Suit exposing (toColor)

import Color exposing (Color)
import Mezzo.Data.Card exposing (Suit)


toColor : Suit -> Color
toColor suit =
    case ( suit.red, suit.blue, suit.yellow ) of
        --yellow
        ( False, False, True ) ->
            Color.rgb255 212 202 106

        --blue
        ( False, True, False ) ->
            Color.rgb255 73 109 137

        --green
        ( False, True, True ) ->
            Color.rgb255 138 189 95

        --red
        ( True, False, False ) ->
            Color.rgb255 190 95 124

        --orange
        ( True, False, True ) ->
            Color.rgb255 212 168 106

        --violette
        ( True, True, False ) ->
            Color.rgb255 107 78 144

        --black
        ( True, True, True ) ->
            Color.rgb255 0 0 0

        --white
        ( False, False, False ) ->
            Color.rgb255 255 255 255
