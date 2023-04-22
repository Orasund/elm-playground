module Pigment exposing (..)

import Color exposing (Color)


type alias Pigment =
    { blue : Bool
    , yellow : Bool
    , red : Bool
    }


color : Bool -> Pigment -> Color
color isPrimary pigment =
    let
        c =
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
    in
    if isPrimary then
        darken c

    else
        c


white : Color
white =
    Color.rgb255 204 211 167


yellow : Color
yellow =
    Color.rgb255 224 169 63


red : Color
red =
    Color.rgb255 202 73 28


orange : Color
orange =
    Color.rgb255 195 117 34


violet : Color
violet =
    Color.rgb255 112 102 95


brown : Color
brown =
    Color.rgb255 146 62 28


black : Color
black =
    Color.rgb255 42 36 44


gray : Color
gray =
    Color.rgb255 188 175 158


darken : Color -> Color
darken c =
    c
        |> Color.toHsla
        |> (\record ->
                { record
                    | lightness = record.lightness * 0.8
                    , saturation = record.saturation * 1.2
                }
           )
        |> Color.fromHsla
