module View.Cell exposing (..)

import Cell exposing (Building(..), Cell)
import Color exposing (Color)
import Config
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import View.Color


base : List (Attribute msg) -> Html msg -> Html msg
base attrs =
    Layout.el
        ([ Html.Attributes.style "border" "1px solid black"
         , Html.Attributes.style "width" (String.fromInt Config.cellSize ++ "px")
         , Html.Attributes.style "height" (String.fromInt Config.cellSize ++ "px")
         , Html.Attributes.style "box-sizing" "border-box"
         ]
            ++ Layout.centered
            ++ attrs
        )


circle : List (Attribute msg) -> Html msg -> Html msg
circle attrs =
    sqaure (Html.Attributes.style "border-radius" "100%" :: attrs)


sqaure : List (Attribute msg) -> Html msg -> Html msg
sqaure attrs =
    Layout.el
        ([ Html.Attributes.style "width" (String.fromInt (Config.cellSize - 10) ++ "px")
         , Html.Attributes.style "height" (String.fromInt (Config.cellSize - 10) ++ "px")
         ]
            ++ attrs
        )


toHtml : Cell -> Html msg
toHtml cell =
    case cell.building of
        Producer color ->
            producer color

        Consumer color ->
            consumer color

        Pipe ->
            pipe


empty : Html msg
empty =
    Layout.none
        |> base []


consumer : Color -> Html msg
consumer color =
    Layout.none
        |> circle
            [ Html.Attributes.style "border"
                ("5px solid " ++ View.Color.toString color)
            ]
        |> base [ Html.Attributes.style "background-color" "gray" ]


producer : Color -> Html msg
producer color =
    Layout.none
        |> circle
            [ Html.Attributes.style "border"
                ("5px solid " ++ View.Color.toString color)
            , Html.Attributes.style "background-color"
                (View.Color.toString color)
            ]
        |> base [ Html.Attributes.style "background-color" "gray" ]


pipe : Html msg
pipe =
    Layout.none
        |> base [ Html.Attributes.style "background-color" "gray" ]
