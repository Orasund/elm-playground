module View.Bubble exposing (..)

import Color
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout
import View.Variant as Variant


asAttrs : List (Attribute msg)
asAttrs =
    Layout.centered
        ++ [ Html.Attributes.style "border-radius" "32px"
           , Html.Attributes.style "background-color" Color.darkTransparent
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
               , Html.Attributes.style "border" ("4px solid " ++ Color.darkTransparent)
               ]
            ++ attrs
        )


specialButton :
    List (Attribute msg)
    -> { label : String, onPress : Maybe msg }
    -> String
    -> Html msg
specialButton attrs args string =
    [ Layout.text [] string
    , shinyCover

    --, starIcon
    ]
        |> Html.div
            (asAttrs
                --  ++ [ Html.Attributes.style "background-color" "transparent" ]
                ++ Layout.asButton args
                ++ asSpecialAttrs
                ++ attrs
            )


starIcon : Html msg
starIcon =
    Layout.text
        [ Html.Attributes.style "filter" "brightness(0)"
        , Html.Attributes.style "color" "rgba(0,0,0,0.1)"
        ]
        "⭐️"
        |> Layout.el
            ([ Html.Attributes.style "border-radius" "100%"
             , Html.Attributes.style "background-color" "yellow"
             , Html.Attributes.style "font-size" "12px"
             , Html.Style.positionAbsolute
             , Html.Attributes.style "padding" "4px"
             , Html.Style.bottom "-16px"
             , Html.Style.width "16px"
             , Html.Style.height "16px"
             ]
                ++ Layout.centered
            )


newLabel : Html msg
newLabel =
    Layout.text
        [ Html.Attributes.style "padding" "4px 8px"
        , Html.Attributes.style "border-radius" "8px"
        , Html.Attributes.style "background-color" "yellow"
        , Html.Attributes.style "font-size" "12px"
        , Html.Style.positionAbsolute
        , Html.Style.top "-8px"
        , Html.Style.left "-8px"
        ]
        "new"


asSpecialAttrs : List (Attribute msg)
asSpecialAttrs =
    [ Layout.asEl
    , Html.Style.positionRelative

    -- , Html.Attributes.style "border" "4px solid yellow"
    ]


shinyCover : Html msg
shinyCover =
    Layout.el
        (asAttrs
            ++ [ Html.Style.positionAbsolute
               , Html.Style.top "-4px"
               , Html.Style.width "100%"
               ]
            ++ Variant.royal
        )
        Layout.none


special :
    List (Attribute msg)
    -> String
    -> Html msg
special attrs string =
    [ Layout.text [] string
    , shinyCover

    --  , starIcon
    ]
        |> Html.div
            (asAttrs
                ++ asSpecialAttrs
                ++ attrs
            )


new :
    List (Attribute msg)
    -> String
    -> Html msg
new attrs string =
    [ Layout.text [] string
    , newLabel
    ]
        |> Html.div
            (asAttrs
                ++ [ Layout.asEl
                   , Html.Style.positionRelative
                   ]
                ++ attrs
            )


newAndSpecial :
    List (Attribute msg)
    -> String
    -> Html msg
newAndSpecial attrs string =
    [ Layout.text [] string
    , shinyCover

    --, starIcon
    , newLabel
    ]
        |> Html.div
            (asAttrs
                ++ asSpecialAttrs
                ++ attrs
            )
