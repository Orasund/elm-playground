module View.Variant exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout


royal : List (Attribute msg) -> Html msg
royal attrs =
    Layout.el
        ([ Html.Attributes.style "border-radius" "100%"
         , Html.Style.height "100%"
         , Html.Style.boxSizingBorderBox
         , Html.Style.positionAbsolute
         , Html.Style.top "0px"
         , Html.Style.width "100%"
         , Html.Attributes.class "animated-background"
         , Html.Attributes.style "background" "linear-gradient(-45deg, rgba(255,255,0,0),rgba(255,255,0,0),rgba(255,255,0,0.5),rgba(255,255,0,0),rgba(255,255,0,0))"
         , Html.Attributes.style "background-size" "400% 400%"
         ]
            ++ attrs
        )
        Layout.none
