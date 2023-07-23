module View exposing (..)

import Cell exposing (Cell(..), Cell1, Cell2)
import Dict
import Grid exposing (Grid(..))
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs =
    Layout.column
        ([ Html.Attributes.style "padding" "1rem"
         , Html.Attributes.style "border" "1px solid black"
         , Html.Attributes.style "background-color" "white"
         , Layout.gap 8
         ]
            ++ attrs
        )


tile1 : Cell1 -> Html msg
tile1 cell =
    cell
        |> Cell.cell1ToEmoji
        |> Layout.text []


tile2 : Cell2 -> Html msg
tile2 cell =
    case cell of
        Connection c ->
            cell
                |> Cell.cell2ToEmoji
                |> Layout.text
                    [ Html.Attributes.style "transform"
                        ("rotate(" ++ String.fromInt (c.rotation * 90) ++ "deg)")
                    ]

        _ ->
            cell
                |> Cell.cell2ToEmoji
                |> Layout.text []


tile : List (Attribute msg) -> ( Int, Int ) -> Grid -> Html msg
tile attrs ( x, y ) grid =
    (case grid of
        Stage1 dict ->
            dict
                |> Dict.get ( x, y )
                |> Maybe.map tile1

        Stage2 dict ->
            dict
                |> Dict.get ( x, y )
                |> Maybe.map tile2
    )
        |> Maybe.withDefault Layout.none
        |> Layout.el
            ([ Html.Attributes.style "width" "64px"
             , Html.Attributes.style "height" "64px"
             , Html.Attributes.style "font-size" "56px"
             ]
                ++ Layout.centered
                ++ attrs
            )
