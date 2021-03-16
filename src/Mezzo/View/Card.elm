module Mezzo.View.Card exposing (view, viewSmall, viewBackSmall)

import Color
import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Mezzo.Data.Card as Card exposing (Card, CardSort(..))
import Mezzo.View.PartBubble as PartBubble
import Mezzo.View.Suit as Suit
import Widget as Widget
import Widget.Customize
import Widget.Material as Material
import Widget.Material.Color as MaterialColor


viewBackSmall : Element msg
viewBackSmall =
    [ Element.none
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
                ++ (Color.rgb255 16 16 16
                        |> MaterialColor.textAndBackground
                   )
            )
    , Element.none
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
                ++ (Color.rgb255 16 16 16
                        |> MaterialColor.textAndBackground
                   )
            )
    ]
        |> Element.column [ Element.centerX ]


viewSmall : Card -> Element msg
viewSmall card =
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
                    [ PartBubble.view [] a
                    , PartBubble.view [] b
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
