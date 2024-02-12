module View.Square exposing (..)

import Color
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout
import View.Variant as Variant


default : List (Attribute msg) -> String -> Html msg
default attrs string =
    Layout.text [] string
        |> Layout.el
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

         -- , Html.Attributes.style "background-color" "rgba(255,255,255,0.6)"
         ]
            ++ attrs
        )


revealedAndSpecialCaptured : List (Attribute msg) -> String -> Html msg
revealedAndSpecialCaptured attrs =
    revealedAndCaptured (Variant.royal ++ attrs)
