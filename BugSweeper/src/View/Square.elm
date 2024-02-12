module View.Square exposing (..)

import Color
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout


default : List (Attribute msg) -> String -> Html msg
default attrs =
    Layout.text
        (Layout.centered
            ++ [ Html.Attributes.style "transition" "border-radius 1s, background-color 0.5s"
               , Html.Attributes.style "height" "64px"
               , Html.Attributes.style "width" "64px"
               , Html.Attributes.style "border-radius" "8px"
               , Html.Attributes.style "background-color" "white"
               , Html.Attributes.style "font-size" "30px"
               , Html.Style.boxSizingBorderBox
               ]
            ++ attrs
        )


revealed : List (Attribute msg) -> String -> Html msg
revealed attrs =
    default
        (Html.Attributes.style "background-color" Color.lightTransparent
            :: attrs
        )


revealedAndCaptured : List (Attribute msg) -> String -> Html msg
revealedAndCaptured attrs =
    revealed
        ([ Html.Attributes.style "border-radius" "100%"
         , Html.Attributes.style "background-color" "rgba(251,244,197,0.6)"
         , Html.Attributes.style "filter" "drop-shadow(0px 0px 8px rgb(251,244,197))"
         ]
            ++ attrs
        )
