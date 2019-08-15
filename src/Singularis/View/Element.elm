module Singularis.View.Element exposing (black, menu, section, slider, subsection, title, white)

import Color
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Element.Input as Input
import Element.Region as Region
import Singularis.View as View exposing (maxScreenWidth)


comfortaaFont : Font
comfortaaFont =
    Font.external
        { name = "Comfortaa"
        , url = "https://fonts.googleapis.com/css?family=Comfortaa&display=swap"
        }


black : Color
black =
    Element.fromRgb <| Color.toRgba <| Color.black


white : Color
white =
    Element.fromRgb <| Color.toRgba <| Color.white


menu : Float -> List { name : String, url : String } -> Element msg
menu scale =
    List.map
        (\{ name, url } ->
            Element.link
                [ Font.family <|
                    [ comfortaaFont
                    , Font.sansSerif
                    ]
                ]
            <|
                { url = url
                , label = Element.text <| name
                }
        )
        >> Element.row
            [ Element.width <| Element.px <| round <| (*) scale <| maxScreenWidth
            , Element.centerX
            , Element.spacing <| round <| (*) scale <| 20
            ]
        >> Element.el
            [ Background.color <| black
            , Font.color <| white
            , Element.width <| Element.fill
            , Element.padding <| round <| (*) scale <| 10
            ]


heading : Int -> String -> Element msg
heading size text =
    Element.el
        [ Font.size size
        , Font.family <|
            [ comfortaaFont
            , Font.sansSerif
            ]
        , Element.centerX
        ]
    <|
        Element.text text


slider : Float -> { onChange : Float -> msg, label : String, min : Float, max : Float, value : Float } -> Element msg
slider scale { onChange, label, min, max, value } =
    Element.row [ Element.width <| Element.fill, Element.spacing <| round <| (*) scale <| 10 ] <|
        [ Element.text <| label
        , Input.slider
            [ Element.behindContent <|
                Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color black
                    ]
                <|
                    Element.none
            , Element.width <| Element.fill
            ]
            { onChange = onChange
            , label =
                Input.labelLeft [] <|
                    Element.text <|String.fromFloat <|
                        (toFloat <| truncate <| 10 * value)
                            / 10
            , min = min
            , max = max
            , value = value
            , thumb = Input.defaultThumb
            , step = Nothing
            }
        ]


title : Float -> String -> Element msg
title scale =
    heading <| round <| (*) scale <| 90


section : Float -> String -> Element msg
section scale =
    heading <| round <| (*) scale <| 45


subsection : Float -> String -> Element msg
subsection scale =
    heading <| round <| (*) scale <| 30
