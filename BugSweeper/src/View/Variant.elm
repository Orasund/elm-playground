module View.Variant exposing (..)

import Html exposing (Attribute)
import Html.Attributes


royal : List (Attribute msg)
royal =
    [ Html.Attributes.class "animated-background"
    , Html.Attributes.style "background" "linear-gradient(-45deg, rgba(255,255,0,0),rgba(255,255,0,0),rgba(255,255,0,0.5),rgba(255,255,0,0),rgba(255,255,0,0))"
    , Html.Attributes.style "background-size" "400% 400%"
    ]
