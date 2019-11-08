module Framework.Button exposing (simple)

import Element exposing (Attribute)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Card as Card
import Framework.Color as Color
import Html.Attributes as Attributes


simple : List (Attribute msg)
simple =
    Card.simple
        ++ [ Font.center
           , Background.color <| Color.lightGrey
           , Element.mouseOver
                [ Border.color <| Color.grey
                ]
           , Element.paddingXY 16 12
           ]
