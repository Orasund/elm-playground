module Framework.Grid exposing (section, simple, spacedEvenly)

import Element exposing (Attribute)
import Element.Border as Border
import Framework.Color as Color


simple : List (Attribute msg)
simple =
    [ Element.spacing 10
    , Element.width Element.fill
    , Element.alignTop
    ]


spacedEvenly : List (Attribute msg)
spacedEvenly =
    [ Element.spaceEvenly
    , Element.width Element.fill
    , Element.height Element.fill
    , Element.centerX
    , Element.centerY
    ]


section : List (Attribute msg)
section =
    simple
        ++ [ Border.widthEach
                { bottom = 0
                , left = 0
                , right = 0
                , top = 2
                }
           , Border.color <| Color.lightGrey
           , Element.paddingEach
                { bottom = 30
                , left = 0
                , right = 0
                , top = 10
                }
           ]
