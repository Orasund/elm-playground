module View exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs =
    Layout.column
        ([ Html.Attributes.style "padding" "1rem"
         , Html.Attributes.style "border" "1px solid black"
         , Html.Attributes.style "background-color" "white"
         , Layout.gap 8
         ]
            ++ attrs
        )
