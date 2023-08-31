module View.Cell exposing (..)

import Cell exposing (Cell)
import Config
import Crop
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


toHtml : List (Attribute msg) -> Cell -> Html msg
toHtml attrs cell =
    let
        green =
            "green"

        backgroundColor =
            "color-mix(in lab," ++ green ++ " " ++ String.fromFloat (100 / (toFloat (Config.maxSoilHealth - cell.soilHealth) + 1)) ++ "%,black " ++ String.fromFloat (100 / (toFloat cell.soilHealth + 1)) ++ "%)"
    in
    (case cell.crop of
        Just crop ->
            (crop |> Crop.toEmoji)
                |> Layout.text
                    [ ((toFloat cell.age + 1)
                        * 3
                        / toFloat (Crop.maxAge crop)
                        |> String.fromFloat
                      )
                        ++ "rem"
                        |> Html.Attributes.style "font-size"
                    ]

        Nothing ->
            Layout.none
    )
        |> square (Html.Attributes.style "background-color" backgroundColor :: attrs)


square : List (Attribute msg) -> Html msg -> Html msg
square attrs =
    Layout.el
        ([ Html.Attributes.style "width" "50px"
         , Html.Attributes.style "height" "50px"
         , Html.Attributes.style "border" "1px solid black"
         ]
            ++ Layout.centered
            ++ attrs
        )
