module Singularis.View.Element exposing (black, menu,subsection, section, title, white)

import Color
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Font as Font exposing (Font)
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


menu : List {name:String, url:String} -> Element msg
menu =
    List.map (\{name, url} -> 
        Element.link [] <|
            {url = url
            ,label = Element.text <| name}
    )
        >> Element.row
            [ Element.width <| Element.px <| maxScreenWidth
            , Element.centerX
            , Element.spacing 20
            ]
        >> Element.el
            [ Background.color <| black
            , Font.color <| white
            , Element.width <| Element.fill
            , Element.padding 10
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


title : String -> Element msg
title =
    heading 90


section : String -> Element msg
section =
    heading 45

subsection : String -> Element msg
subsection =
    heading 30