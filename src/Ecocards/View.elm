module Ecocards.View exposing (squareCard)

import Color exposing (Color)
import Ecocards.View.Material exposing (..)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attributes
import Widget
import Widget.Style exposing (ButtonStyle, ColumnStyle, DialogStyle, ExpansionPanelStyle, LayoutStyle, RowStyle, SortTableStyle, TabStyle, TextInputStyle)
import Widget.Style.Material as Material exposing (Palette)



---


squareCard :
    { header : ( String, String )
    , text : String
    , footer : String
    , onPress : Maybe msg
    , color : Maybe Color
    }
    -> Element msg
squareCard { header, footer, onPress, text, color } =
    let
        ( headerLeft, headerRight ) =
            header
    in
    Input.button
        ([ Element.height <| Element.px 32
         , Element.padding 4
         , Border.rounded <| 8
         ]
            ++ buttonFont
            ++ (if onPress == Nothing then
                    [ Element.htmlAttribute <| Attributes.style "cursor" "not-allowed"
                    , Color.rgb255 127 127 127
                        |> scaleOpacity 0.12
                        |> fromColor
                        |> Border.color
                    , Border.width 1
                    ]

                else
                    color
                        |> Maybe.map
                            (scaleOpacity 0.36
                                >> fromColor
                                >> Background.color
                                >> List.singleton
                            )
                        |> Maybe.withDefault
                            [ Color.rgb255 127 127 127
                                |> scaleOpacity 0.12
                                |> fromColor
                                |> Background.color
                            ]
               )
            ++ [ Element.width <| Element.px <| 70
               , Element.height <| Element.px <| 70
               , Element.padding <| 5
               ]
        )
        { onPress = onPress
        , label =
            [ [ headerLeft
                    |> Element.text
              , headerRight
                    |> Element.text
              ]
                |> Element.row [ Element.width <| Element.fill, Element.spaceEvenly, Font.size 12 ]
            , text
                |> Element.text
                |> Element.el
                    [ Element.centerX
                    , Font.size 32
                    ]
            , [ footer
                    |> Element.text
                    |> Element.el [ Font.size 8 ]
              , Element.text "?" |> Element.el []
              ]
                |> Element.row [ Element.width <| Element.fill, Element.spaceEvenly ]
            ]
                |> Element.column
                    [ Element.width <| Element.fill
                    , Element.height <| Element.fill
                    , Element.spaceEvenly
                    ]
        }
