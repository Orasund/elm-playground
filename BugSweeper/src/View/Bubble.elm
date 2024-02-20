module View.Bubble exposing (..)

import Color
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout
import View.Variant as Variant


asAttrs : List (Attribute msg)
asAttrs =
    [ Html.Style.justifyContentCenter
    , Html.Style.alignItemsCenter
    , Html.Style.borderRadius "32px"
    , Html.Style.backgroundColor Color.darkTransparent
    , Html.Style.heightPx 48
    , Html.Style.minWidthPx 48
    , Html.Style.widthFitContent
    , Html.Style.paddingPx 8
    , Html.Style.boxSizingBorderBox
    , Html.Style.fontSizePx 20
    , Html.Style.border "4px solid transparent"
    , Html.Attributes.class "emoji-color-font"
    ]


default : List (Attribute msg) -> String -> Html msg
default attrs =
    Layout.divText
        (asAttrs
            ++ attrs
        )


unkown : List (Attribute msg) -> String -> Html msg
unkown attrs =
    Layout.divText
        (asAttrs
            ++ [ Html.Style.filter "brightness(0)"
               , Html.Style.color "rgba(0,0,0,0.2)"
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
            ++ [ Html.Style.backgroundColor "transparent"
               , Html.Style.border ("4px solid " ++ Color.darkTransparent)
               ]
            ++ attrs
        )


specialButton :
    List (Attribute msg)
    -> { label : String, onPress : Maybe msg }
    -> String
    -> Html msg
specialButton attrs args string =
    [ Layout.divText [] string
    , Variant.royal []
    , starIcon
    ]
        |> Html.div
            (asAttrs
                ++ [ Html.Style.backgroundColor "transparent"
                   , Html.Style.border ("4px solid " ++ Color.darkTransparent)
                   ]
                ++ Layout.asButton args
                ++ asSpecialAttrs
                ++ attrs
            )


starIcon : Html msg
starIcon =
    Layout.divText
        [ Html.Style.filter "brightness(0)"
        , Html.Style.color "rgba(0,0,0,0.1)"
        ]
        "⭐️"
        |> Layout.divWrapper
            [ Html.Style.borderRadius "100%"
            , Html.Style.fontSizePx 8
            , Html.Style.positionAbsolute
            , Html.Style.bottomPx 4
            , Html.Style.justifyContentCenter
            , Html.Style.alignContentCenter
            ]


newLabel : Html msg
newLabel =
    Layout.divText
        [ Html.Style.padding "4px 8px"
        , Html.Style.borderRadius "8px"
        , Html.Style.backgroundColor "yellow"
        , Html.Style.fontSizePx 12
        , Html.Style.positionAbsolute
        , Html.Style.topPx -8
        , Html.Style.leftPx -8
        ]
        "new"


asSpecialAttrs : List (Attribute msg)
asSpecialAttrs =
    [ Html.Style.positionRelative
    ]


special :
    List (Attribute msg)
    -> String
    -> Html msg
special attrs string =
    [ Layout.divText [] string
    , Variant.royal []
    , starIcon
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
    [ Layout.divText [] string
    , newLabel
    ]
        |> Html.div
            (Html.Style.positionRelative
                :: asAttrs
                ++ attrs
            )


newAndSpecial :
    List (Attribute msg)
    -> String
    -> Html msg
newAndSpecial attrs string =
    [ Layout.divText [] string
    , Variant.royal []
    , starIcon
    , newLabel
    ]
        |> Html.div
            (asAttrs
                ++ asSpecialAttrs
                ++ attrs
            )
