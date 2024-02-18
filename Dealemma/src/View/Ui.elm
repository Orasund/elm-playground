module View.Ui exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout


button : List (Attribute msg) -> { onPress : Maybe msg, label : String } -> Html msg
button attrs args =
    Layout.divText
        ([ Html.Style.displayFlex
         , Html.Style.boxSizingBorderBox
         , Html.Attributes.style "border" "4px solid #679aff"
         , Html.Attributes.style "padding" "8px 16px"
         , Html.Attributes.style "background-color" "transparent"
         , Html.Attributes.style "font-weight" "bold"
         ]
            ++ Layout.asButton args
            ++ attrs
        )
        args.label
