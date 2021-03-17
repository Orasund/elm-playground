module Mezzo.View.Stack exposing (view)

import Color
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Mezzo.Data.Card as Card exposing (Card, CardSort(..))
import Mezzo.View.Suit as Suit
import Widget as Widget
import Widget.Customize
import Widget.Material as Material
import Widget.Material.Color as MaterialColor
import Widget.Material.Typography as Typography


viewSheet : Card -> Element msg
viewSheet card =
    let
        color =
            case card.suit of
                ( suit, Nothing ) ->
                    suit |> Suit.toColor

                ( _, Just _ ) ->
                    Color.rgb255 240 240 240
    in
    Element.none
        |> Element.el
            [ color
                |> Color.toRgba
                |> Element.fromRgb
                |> Background.color
            , Element.width <| Element.px 64
            , Element.height <| Element.px 4
            ]


view : List Card -> Element msg
view list =
    if list |> List.isEmpty then
        Element.none

    else
        [ list
            |> List.length
            |> String.fromInt
            |> Element.text
            |> Element.el Typography.h6
        , list
            |> List.map viewSheet
            |> Element.column []
        ]
            |> Element.row [ Element.spacing 4 ]
