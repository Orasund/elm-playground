module Mezzo.View.PartBubble exposing (view, viewJoined)

import Color
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Mezzo.Data.Card as Card exposing (CardPart, CardSort(..))
import Mezzo.View.Suit as Suit
import Widget as Widget
import Widget.Customize
import Widget.Material as Material
import Widget.Material.Color as MaterialColor


view : List (Attribute msg) -> CardPart -> Element msg
view attr card =
    [ (case card.sort of
        Valued n ->
            n |> String.fromInt

        Add ->
            "Add"
      )
        |> Element.text
        |> Element.el [ Element.centerX, Element.centerY ]
    , (card.suit
        |> Card.suitToString
        |> String.uncons
        |> Maybe.map (Tuple.first >> String.fromChar)
        |> Maybe.withDefault ""
      )
        |> Element.text
        |> Element.el
            [ Element.centerX
            ]
    ]
        |> Element.column
            [ Element.centerX
            , Element.height <| Element.fill
            , Element.spaceEvenly
            ]
        |> Element.el
            ([ Element.width <| Element.px 32
             , Element.height <| Element.px 32
             , Border.width 1
             , Element.rgb255 63 63 63
                |> Border.color
             , Element.padding 4
             , Font.size 10
             , Border.rounded 32
             ]
                ++ (card.suit
                        |> Suit.toColor
                        |> MaterialColor.textAndBackground
                   )
                ++ attr
            )


viewJoined : ( CardPart, CardPart ) -> Element msg
viewJoined ( first, second ) =
    [ first
        |> view
            [ Element.moveRight 4
            ]
    , second
        |> view
            [ Element.moveLeft 4
            ]
    ]
        |> Element.row
            [ Element.centerX
            ]
