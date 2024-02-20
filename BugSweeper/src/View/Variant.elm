module View.Variant exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout


royal : List (Attribute msg) -> Html msg
royal attrs =
    Layout.divWrapper
        ([ Html.Style.borderRadius "100%"
         , Html.Style.height "100%"
         , Html.Style.boxSizingBorderBox
         , Html.Style.positionAbsolute
         , Html.Style.topPx 0
         , Html.Style.width "100%"
         , Html.Style.background "linear-gradient(-45deg, rgba(255,255,0,0),rgba(255,255,0,0),rgba(255,255,0,0.5),rgba(255,255,0,0),rgba(255,255,0,0))"
         , Html.Style.backgroundSize "400% 400%"
         , Html.Style.border "4px solid rgba(255,255,0,0.5)"
         , Html.Attributes.class "animated-background"
         ]
            ++ attrs
        )
        Layout.none
