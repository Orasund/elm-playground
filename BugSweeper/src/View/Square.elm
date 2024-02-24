module View.Square exposing (..)

import Color
import Html exposing (Attribute, Html)
import Html.Style
import Layout
import View.Variant as Variant


asAttrs : List (Attribute msg)
asAttrs =
    [ Html.Style.displayFlex
    , Html.Style.justifyContentCenter
    , Html.Style.alignItemsCenter
    , Html.Style.transition "border-radius 1s, background-color 0.5s"
    , Html.Style.heightPx 64
    , Html.Style.widthPx 64
    , Html.Style.borderRadius "8px"
    , Html.Style.backgroundColor "white"
    , Html.Style.fontSizePx 30
    , Html.Style.boxSizingBorderBox
    ]


default : List (Attribute msg) -> String -> Html msg
default attrs string =
    Layout.divText [] string
        |> Layout.divWrapper
            (asAttrs
                ++ attrs
            )


revealed : List (Attribute msg) -> String -> Html msg
revealed attrs =
    default
        (Html.Style.backgroundColor Color.lightTransparent
            :: attrs
        )


revealedAndCaptured : List (Attribute msg) -> String -> Html msg
revealedAndCaptured attrs =
    revealed
        (Html.Style.borderRadius "100%"
            :: attrs
        )


revealedAndSpecialCaptured : List (Attribute msg) -> String -> Html msg
revealedAndSpecialCaptured attrs string =
    [ Layout.divText [] string
    , Variant.royal []
    ]
        |> Html.div
            (asAttrs
                ++ [ Html.Style.displayFlex
                   , Html.Style.positionRelative
                   , Html.Style.borderRadius "100%"
                   , Html.Style.backgroundColor Color.lightTransparent
                   ]
                ++ attrs
            )
