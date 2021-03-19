module Mezzo.View.Stack exposing (view)

import Color
import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Font as Font
import Mezzo.Data.Card as Card exposing (Card, CardSort(..))
import Mezzo.View.Suit as Suit
import String
import Widget.Material.Color as MaterialColor
import Widget.Material.Typography as Typography


viewSheet : List (Attribute msg) -> Card -> Element msg
viewSheet attr card =
    let
        ( color, string ) =
            case card.suit of
                ( suit, Nothing ) ->
                    ( suit |> Suit.toColor
                    , suit |> Card.suitToString
                    )

                ( _, Just _ ) ->
                    ( Color.rgb255 240 240 240
                    , ""
                    )
    in
    (string
        |> String.uncons
        |> Maybe.map (Tuple.first >> String.fromChar)
        |> Maybe.withDefault ""
    )
        |> Element.text
        |> Element.el [ Element.alignRight ]
        |> Element.el
            ([ Element.width <| Element.px 64
             , Element.padding 2
             , Font.size 10
             ]
                ++ (color
                        |> MaterialColor.textAndBackground
                   )
                ++ attr
            )


view : List Card -> Element msg
view list =
    let
        rounded =
            4
    in
    if list |> List.isEmpty then
        Element.none

    else
        let
            length =
                list
                    |> List.length
        in
        [ length
            |> String.fromInt
            |> Element.text
            |> Element.el Typography.h6
        , list
            |> List.indexedMap
                (\i ->
                    viewSheet
                        (if length == 1 then
                            [ Border.rounded rounded ]

                         else if i == 0 then
                            [ Border.roundEach
                                { topLeft = rounded
                                , topRight = rounded
                                , bottomLeft = 0
                                , bottomRight = 0
                                }
                            ]

                         else if i == length - 1 then
                            [ Border.roundEach
                                { topLeft = 0
                                , topRight = 0
                                , bottomLeft = rounded
                                , bottomRight = rounded
                                }
                            ]

                         else
                            []
                        )
                )
            |> Element.column []
        ]
            |> Element.row [ Element.spacing 4 ]
