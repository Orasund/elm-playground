module Framework.Section exposing (simple)

import Element exposing (Attribute)
import Element.Border as Border
import Framework.Color as Color


simple : List (Attribute msg)
simple =
    [ Border.widthEach
        { bottom = 2
        , left = 0
        , right = 0
        , top = 0
        }
    , Border.color <| Color.lightGrey
    , Element.paddingEach
        { bottom = 30
        , left = 0
        , right = 0
        , top = 10
        }
    , Element.spacing 10
    , Element.width Element.fill
    ]
