module LittleWorldPuzzler.View.Button exposing (view)

import Element exposing (Attribute, Element)
import Element.Input as Input
import Framework.Button as Button


view :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
view attributes body =
    Input.button
        (Button.simple
            ++ attributes
        )
        body
