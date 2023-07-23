module View.Svg exposing (..)

import Cell exposing (Cell1)
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes


grid : { width : Int, height : Int } -> Dict ( Int, Int ) Cell1 -> Svg msg
grid args dict =
    dict
        |> Dict.toList
        |> List.map (\( ( x, y ), cell ) -> ( ( x + 1, y + 1 ), cell |> Cell.cell1ToColor ))
        |> fromPixels { width = args.width, height = args.height, size = 6 }


cell1 : { width : Int, height : Int } -> String -> Svg msg
cell1 args color =
    [ ( ( 0, 0 ), color ) ]
        |> fromPixels { width = args.width, height = args.height, size = 1 }


fromPixels : { width : Int, height : Int, size : Int } -> List ( ( Int, Int ), String ) -> Svg msg
fromPixels args pixels =
    let
        canvasSize =
            120

        pixelSize =
            canvasSize // args.size
    in
    pixels
        |> List.map
            (\( ( x, y ), color ) ->
                Svg.rect
                    [ Svg.Attributes.x (x * pixelSize |> String.fromInt)
                    , Svg.Attributes.y (y * pixelSize |> String.fromInt)
                    , Svg.Attributes.width (pixelSize |> String.fromInt)
                    , Svg.Attributes.height (pixelSize |> String.fromInt)
                    , Svg.Attributes.fill color
                    ]
                    []
            )
        |> Svg.svg
            [ Svg.Attributes.width (String.fromInt args.width)
            , Svg.Attributes.height (String.fromInt args.height)
            , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt canvasSize ++ " " ++ String.fromInt canvasSize)
            ]
