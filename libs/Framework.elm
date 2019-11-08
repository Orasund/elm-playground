module Framework exposing (container, layout, layoutAttributes, layoutOptions)

import Element exposing (Attribute, Element, Option)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Framework.Color as Color
import Html exposing (Html)


container : List (Attribute msg)
container =
    [ Element.centerX
    , Element.centerY
    , Element.width (Element.fill |> Element.maximum 1200)
    , Element.padding <| 20
    , Region.mainContent
    , Background.color <| Element.rgb255 255 255 255
    ]


layout : List (Attribute msg) -> Element msg -> Html msg
layout attributes =
    Element.layoutWith
        { options = layoutOptions
        }
        (layoutAttributes ++ attributes)


layoutOptions : List Option
layoutOptions =
    Element.focusStyle
        { borderColor = Just Color.turquoise
        , backgroundColor = Nothing
        , shadow =
            Just <|
                { blur = 10
                , color = Color.turquoise
                , offset = ( 0, 0 )
                , size = 1
                }
        }
        |> List.singleton


layoutAttributes : List (Attribute msg)
layoutAttributes =
    [ Font.size 16
    , Font.color <| Color.darkerGrey
    ]
        ++ Color.light
