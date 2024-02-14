module View.Square exposing (..)

import Color
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout
import View.Variant as Variant


asAttrs : List (Attribute msg)
asAttrs =
    Layout.centered
        ++ [ Html.Attributes.style "transition" "border-radius 1s, background-color 0.5s"
           , Html.Attributes.style "height" "64px"
           , Html.Attributes.style "width" "64px"
           , Html.Attributes.style "border-radius" "8px"
           , Html.Attributes.style "background-color" "white"
           , Html.Attributes.style "font-size" "30px"
           , Html.Style.boxSizingBorderBox
           ]


default : List (Attribute msg) -> String -> Html msg
default attrs string =
    Layout.text [] string
        |> Layout.el
            (asAttrs
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
        (Html.Attributes.style "border-radius" "100%"
            :: attrs
        )


revealedAndSpecialCaptured : List (Attribute msg) -> String -> Html msg
revealedAndSpecialCaptured attrs string =
    [ Layout.text [] string
    , Variant.royal []
    ]
        |> Html.div
            (asAttrs
                ++ [ Layout.asEl
                   , Html.Style.positionRelative
                   , Html.Attributes.style "border-radius" "100%"
                   , Html.Attributes.style "background-color" Color.lightTransparent
                   ]
                ++ attrs
            )
