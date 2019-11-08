module Framework.Input exposing (label, simple)

import Element exposing (Attribute, Element, Option)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Card as Card
import Framework.Color as Color


label : List (Attribute msg)
label =
    [ Element.centerX
    , Element.padding 10
    , Element.height Element.shrink
    , Element.width Element.fill
    ]


simple : List (Attribute msg)
simple =
    [ Background.color <| Color.lighterGrey
    , Font.color <| Color.darkerGrey
    ]
