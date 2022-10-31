module View.Button exposing (..)

import Html exposing (Html)
import Layout


toHtml : msg -> String -> Html msg
toHtml onPress label =
    Html.text label
        |> Layout.buttonEl { onPress = Just onPress, label = label } []
