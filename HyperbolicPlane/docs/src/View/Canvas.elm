module View.Canvas exposing (..)

import Canvas
import Canvas.Settings
import Color
import Html exposing (Html)
import Layout


euclidean : List ( Float, Float ) -> Html msg
euclidean list =
    let
        size =
            200
    in
    list
        |> List.map
            (\( x, y ) ->
                ( x * (size / 2) + size / 2
                , y * (size / 2) + size / 2
                )
            )
        |> List.map (\p -> Canvas.circle p 1)
        |> Canvas.shapes [ Canvas.Settings.fill Color.black ]
        |> List.singleton
        |> Canvas.toHtml ( size, size ) []
        |> Layout.el Layout.centered


hyperbolic : List ( Float, Float ) -> Html msg
hyperbolic list =
    let
        size =
            200
    in
    [ list
        |> List.map
            (\( x, y ) ->
                ( x * (size / 2) + size / 2
                , y * (size / 2) + size / 2
                )
            )
        |> List.map (\p -> Canvas.circle p 1)
        |> Canvas.shapes [ Canvas.Settings.fill Color.black ]
    , Canvas.shapes [ Canvas.Settings.stroke Color.gray ] [ Canvas.circle ( size / 2, size / 2 ) (size / 2) ]
    ]
        |> Canvas.toHtml ( size, size ) []
        |> Layout.el Layout.centered
