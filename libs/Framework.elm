module Framework exposing (layout, layoutAttributes, layoutOptions)

import Element exposing (Attribute, Element, Option)
import Element.Background as Background
import Element.Font as Font
import Framework.Color as Color
import Html exposing (Html)


layout : List (Attribute msg) -> Element msg -> Html msg
layout attributes =
    Element.layoutWith
        { options =
            layoutOptions
        }
        (layoutAttributes ++ attributes)


layoutOptions : List Option
layoutOptions =
    Element.focusStyle
        { borderColor = Just Color.turquoise
        , backgroundColor = Nothing
        , shadow = Nothing
        }
        |> List.singleton


layoutAttributes : List (Attribute msg)
layoutAttributes =
    [ Font.size 16
    , Font.color <|
        Color.darkerGrey
    ]
        ++ Color.light
