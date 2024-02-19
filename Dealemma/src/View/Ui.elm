module View.Ui exposing (..)

import Html exposing (Attribute, Html)
import Html.Style
import Layout


button : List (Attribute msg) -> { onPress : Maybe msg, label : String } -> Html msg
button attrs args =
    Layout.divText
        ([ Html.Style.displayFlex
         , Html.Style.boxSizingBorderBox
         , Html.Style.border "4px solid #679aff"
         , Html.Style.padding "8px 16px"
         , Html.Style.backgroundColor "transparent"
         , Html.Style.fontWeightBold
         ]
            ++ Layout.asButton args
            ++ attrs
        )
        args.label
