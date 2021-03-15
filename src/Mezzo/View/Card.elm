module Mezzo.View.Card exposing (view)

import Element exposing (Element)
import Element.Input as Input
import Mezzo.Data.Card as Card exposing (Card, CardSort(..))
import Mezzo.View.Part as Part
import Mezzo.View.Suit as Suit
import Widget as Widget
import Widget.Customize
import Widget.Material as Material
import Widget.Material.Color as MaterialColor


view : Maybe msg -> Card -> Element msg
view onPress card =
    let
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
        |> Element.el
            [ Element.centerY
            , Element.centerX
            ]
        |> Element.el
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            , Element.paddingXY 16 12
            ]
        |> Widget.asItem
    , [ (Card.suitToString s1
            ++ (if s1 /= s2 then
                    " | "
                        ++ Card.suitToString s2

                else
                    ""
               )
        )
            |> Element.text
            |> Element.el [ Element.centerX ]
      , card
            |> Card.toParts
            |> (\( a, b ) ->
                    [ Part.asBubble [] a
                    , Part.asBubble [] b
                    ]
                        |> Element.row [ Element.spacing 8, Element.centerX ]
               )
            |> Element.el [ Element.centerX ]
      ]
        |> Element.column
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            , Element.paddingXY 16 12
            , Element.spaceEvenly
            ]
        |> Widget.asItem
    ]
        |> Widget.itemList
            (Material.cardColumn Material.defaultPalette
                |> Widget.Customize.elementColumn
                    [ Element.width <| Element.px 150
                    , Element.height <| Element.px 200
                    ]
                |> Widget.Customize.mapContent
                    (Widget.Customize.element
                        [ Element.width <| Element.px 150
                        , Element.height <| Element.fill
                        , Element.padding 0
                        ]
                        >> Widget.Customize.ifFirst
                            (s1
                                |> Suit.toColor
                                |> MaterialColor.textAndBackground
                            )
                        >> Widget.Customize.ifLast
                            (s2
                                |> Suit.toColor
                                |> MaterialColor.textAndBackground
                            )
                    )
            )
        |> (\label ->
                case onPress of
                    Just msg ->
                        Input.button []
                            { onPress = Just msg
                            , label = label
                            }

                    Nothing ->
                        label
           )
