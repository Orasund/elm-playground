module Pigment exposing (..)

import Color exposing (Color)


type alias Pigment =
    { blue : Bool
    , yellow : Bool
    , red : Bool
    }


color : Pigment -> Color
color pigment =
    case ( pigment.blue, pigment.red, pigment.yellow ) of
        ( False, False, False ) ->
            white

        ( False, False, True ) ->
            yellow

        ( False, True, False ) ->
            red

        ( False, True, True ) ->
            orange

        ( True, False, False ) ->
            violet

        ( True, False, True ) ->
            gray

        ( True, True, False ) ->
            brown

        ( True, True, True ) ->
            black


white : Color
white =
    Color.white


yellow : Color
yellow =
    Color.yellow


red : Color
red =
    Color.red


orange : Color
orange =
    Color.orange


violet : Color
violet =
    Color.rgb255 139 104 127


brown : Color
brown =
    Color.brown


black : Color
black =
    Color.black


gray : Color
gray =
    Color.gray
