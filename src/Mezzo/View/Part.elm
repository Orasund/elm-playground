module Mezzo.View.Part exposing (asBubble, view, viewBubbles)

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


asBubble : List (Attribute msg) -> CardPart -> Element msg
asBubble attr card =
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


viewBubbles : ( CardPart, CardPart ) -> Element msg
viewBubbles ( first, second ) =
    [ first
        |> asBubble
            [ Element.moveRight 4
            ]
    , second
        |> asBubble
            [ Element.moveLeft 4
            ]
    ]
        |> Element.row
            [ Element.centerX
            ]


asCard : ( CardPart, CardPart ) -> Element msg
asCard tuple =
    let
        card =
            Card.fromParts tuple

        ( s1, s2 ) =
            card.suit
                |> Tuple.mapSecond (Maybe.withDefault (card.suit |> Tuple.first))
    in
    [ (case card.sort of
        Valued n ->
            n |> String.fromInt

        Add ->
            "Add"
      )
        |> Element.text
        |> Element.el [ Element.centerX, Element.centerY ]
        |> Element.el
            ([ Element.width <| Element.px 80
             , Element.height <| Element.px 60
             , Border.roundEach
                { topLeft = 8
                , topRight = 8
                , bottomLeft = 0
                , bottomRight = 0
                }
             ]
                ++ (s2
                        |> Suit.toColor
                        |> MaterialColor.textAndBackground
                   )
            )
    , (Card.suitToString s2
        ++ (if s1 /= s2 then
                " | "
                    ++ Card.suitToString s1

            else
                ""
           )
      )
        |> Element.text
        |> Element.el [ Element.centerX ]
        |> Element.el
            ([ Element.width <| Element.px 80
             , Element.height <| Element.px 60
             , Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = 8
                , bottomRight = 8
                }
             , Element.paddingXY 16 12
             ]
                ++ (s1
                        |> Suit.toColor
                        |> MaterialColor.textAndBackground
                   )
            )
    ]
        |> Element.column [ Element.centerX ]


view : List CardPart -> Element msg
view list =
    case list |> List.reverse of
        [ first, second ] ->
            [ asCard ( first, second )
            , viewBubbles ( first, second )
            ]
                |> Element.column
                    [ Element.spacing 10
                    ]

        [ first ] ->
            first
                |> asBubble
                    [ Element.alignBottom
                    ]

        _ ->
            Element.none
