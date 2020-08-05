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
    , getInfoMsg : Maybe msg
    , color : Maybe Color
    }
    -> Element msg
squareCard { header, footer, onPress, getInfoMsg, text, color } =
    let
        ( headerLeft, headerRight ) =
            header

        attributes =
            buttonFont
                ++ (if onPress == Nothing then
                        [ Color.rgb255 127 127 127
                            |> scaleOpacity 0.12
                            |> fromColor
                            |> Border.color
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
                   ]
    in
    [ Input.button
        (attributes
            ++ (if onPress == Nothing then
                    [ Element.htmlAttribute <| Attributes.style "cursor" "not-allowed"
                    , Border.widthEach
                        { bottom = 0
                        , left = 1
                        , right = 1
                        , top = 1
                        }
                    ]

                else
                    []
               )
            ++ [ Border.roundEach
                    { topLeft = 8
                    , topRight = 8
                    , bottomLeft = 0
                    , bottomRight = 0
                    }
               , Element.paddingEach
                    { bottom = 0
                    , left = 4
                    , right = 4
                    , top = 4
                    }
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
                    , Font.size 30
                    ]
            ]
                |> Element.column
                    [ Element.width <| Element.fill
                    , Element.height <| Element.px 51
                    , Element.spaceEvenly
                    ]
        }
    , Input.button
        (attributes
            ++ (if onPress == Nothing then
                    [ Border.widthEach
                        { bottom = 1
                        , left = 1
                        , right = 1
                        , top = 0
                        }
                    ]

                else
                    []
               )
            ++ [ Element.height <| Element.px 15
               , Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = 8
                    , bottomRight = 8
                    }
               , Element.paddingEach
                    { bottom = 4
                    , left = 4
                    , right = 4
                    , top = 0
                    }
               ]
        )
        { onPress = getInfoMsg
        , label =
            [ footer
                |> Element.text
                |> Element.el [ Font.size 8, Element.alignBottom ]
            , Element.text "?" |> Element.el []
            ]
                |> Element.row [ Element.width <| Element.fill, Element.spaceEvenly ]
        }
    ]
        |> Element.column
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            , Element.spaceEvenly
            ]
