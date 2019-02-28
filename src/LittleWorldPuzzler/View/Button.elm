module LittleWorldPuzzler.View.Button exposing (view)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


view :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
view attributes body =
    Input.button
        ([ Border.width 1
         , Border.color <| Element.rgba255 219 219 219 1
         , Background.color <| Element.rgb255 255 255 255
         , Element.mouseOver
            [ Border.color <| Element.rgba255 155 203 255 1
            ]
         , Font.center
         ]
            ++ attributes
        )
        body
