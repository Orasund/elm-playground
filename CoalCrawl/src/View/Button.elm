module View.Button exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Layout


toHtml : Maybe msg -> String -> Html msg
toHtml onPress label =
    Html.text label
        |> Layout.buttonEl { onPress = onPress, label = label }
            (case onPress of
                Just _ ->
                    []

                Nothing ->
                    [ Attr.disabled True ]
            )
