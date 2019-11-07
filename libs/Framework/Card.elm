module Framework.Card exposing (basic, fill, large, small)

import Element exposing (Attribute)
import Element.Border as Border
import Framework.Color as Color


basic : List (Attribute msg)
basic =
    [ Border.shadow
        { blur = 10
        , color = Element.rgba 0 0 0 0.05
        , offset = ( 0, 2 )
        , size = 1
        }
    , Border.width 1
    , Border.color Color.lightGrey
    , Border.rounded 4
    , Element.alignTop
    , Element.padding 20
    , Element.height Element.shrink
    ]


withSize : Int -> List (Attribute msg)
withSize int =
    basic ++ [ Element.width (Element.px int) ]


small : List (Attribute msg)
small =
    withSize 240


large : List (Attribute msg)
large =
    withSize 480


fill : List (Attribute msg)
fill =
    basic ++ [ Element.width Element.fill ]
