module View.Bubble exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout


asAttrs : List (Attribute msg)
asAttrs =
    Layout.centered
        ++ [ Html.Attributes.style "border-radius" "32px"
           , Html.Attributes.style "background-color" "rgba(0,0,0,0.1)"
           , Html.Attributes.style "height" "48px"
           , Html.Attributes.style "min-width" "48px"
           , Html.Attributes.style "width" "fit-content"
           , Html.Attributes.style "padding" "8px 8px"
           , Html.Style.boxSizingBorderBox
           , Html.Attributes.style "font-size" "20px"
           , Html.Attributes.class "emoji-color-font"
           ]


default : List (Attribute msg) -> String -> Html msg
default attrs =
    Layout.text
        (asAttrs
            ++ attrs
        )


unkown : List (Attribute msg) -> String -> Html msg
unkown attrs =
    Layout.text
        (asAttrs
            ++ [ Html.Attributes.style "filter" "brightness(0)"
               , Html.Attributes.style "color" "rgba(0,0,0,0.2)"
               ]
            ++ attrs
        )


button :
    List (Attribute msg)
    -> { label : String, onPress : Maybe msg }
    -> String
    -> Html msg
button attrs args =
    default
        (Layout.asButton args
            ++ [ Html.Attributes.style "background-color" "transparent"
               , Html.Attributes.style "border" "4px solid rgba(0,0,0,0.1)"
               ]
            ++ attrs
        )


new :
    List (Attribute msg)
    -> String
    -> Html msg
new attrs string =
    [ Layout.text [] string
    , Layout.text
        [ Html.Attributes.style "padding" "4px 8px"
        , Html.Attributes.style "border-radius" "8px"
        , Html.Attributes.style "background-color" "yellow"
        , Html.Attributes.style "font-size" "12px"
        , Html.Style.positionAbsolute
        , Html.Style.top "-8px"
        , Html.Style.left "-8px"
        ]
        "new"
    ]
        |> Html.div
            (asAttrs
                ++ [ Layout.asEl
                   , Html.Style.positionRelative

                   -- , Html.Attributes.style "border" "4px solid yellow"
                   --, Html.Attributes.style "background-color" "transparent"
                   ]
                ++ attrs
            )
