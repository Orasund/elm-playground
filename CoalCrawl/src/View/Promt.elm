module View.Promt exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Color


fromString : Maybe String -> Html msg
fromString maybe =
    maybe
        |> Maybe.map
            (\s ->
                s
                    |> Html.text
                    |> Layout.el
                        [ Layout.fill
                        , Attr.style "background-color" View.Color.yellow
                        , Attr.style "height" "32px"
                        , Attr.style "padding" "0px 8px"
                        , Layout.alignAtCenter
                        ]
            )
        |> Maybe.withDefault Layout.none
        |> Layout.el [ Layout.alignAtCenter, Attr.style "height" "32px" ]
