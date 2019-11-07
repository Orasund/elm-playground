module Framework.Color exposing
    ( cyan
    , danger
    , dark
    , darkGrey
    , darkerGrey
    , green
    , grey
    , info
    , light
    , lightGrey
    , lighterGrey
    , primary
    , red
    , success
    , turquoise
    , warning
    , yellow
    )

import Element exposing (Attribute, Color)
import Element.Background as Background
import Element.Font as Font


lightGrey : Color
lightGrey =
    Element.rgb255 219 219 219


grey : Color
grey =
    Element.rgb255 122 122 122


darkGrey : Color
darkGrey =
    Element.rgb255 54 54 54


turquoise : Color
turquoise =
    Element.rgb255 0 209 178


primary : List (Attribute msg)
primary =
    turquoise
        |> Background.color
        |> List.singleton


cyan : Color
cyan =
    Element.rgb255 32 156 238


info : List (Attribute msg)
info =
    cyan
        |> Background.color
        |> List.singleton


green : Color
green =
    Element.rgb255 35 209 96


success : List (Attribute msg)
success =
    green
        |> Background.color
        |> List.singleton


yellow : Color
yellow =
    Element.rgb255 255 221 87


warning : List (Attribute msg)
warning =
    yellow
        |> Background.color
        |> List.singleton


red : Color
red =
    Element.rgb255 255 56 96


danger : List (Attribute msg)
danger =
    [ red
        |> Background.color
    , Font.color <| lighterGrey
    ]


lighterGrey : Color
lighterGrey =
    Element.rgb255 245 245 245


light : List (Attribute msg)
light =
    lighterGrey
        |> Background.color
        |> List.singleton


darkerGrey : Color
darkerGrey =
    Element.rgb255 18 18 18


dark : List (Attribute msg)
dark =
    [ darkerGrey |> Background.color
    , Font.color <| lighterGrey
    ]
