module View.Svg exposing (..)

import Cell exposing (Cell)
import Dict exposing (Dict)
import Level exposing (Level)
import RelativePos exposing (RelativePos)
import Svg exposing (Svg)
import Svg.Attributes


type alias RenderFunction msg =
    { pos : ( Int, Int ), color : String, size : Int } -> Svg msg


grid :
    { width : Int
    , height : Int
    , active : ( Int, Int ) -> Bool
    , render : Cell -> RenderFunction msg
    , level : Level
    }
    -> Dict RelativePos Cell
    -> Svg msg
grid args dict =
    dict
        |> Dict.toList
        |> List.map
            (\( ( ( x, y ), _ ), cell ) ->
                { pos = ( x + 1, y + 1 )
                , color =
                    cell
                        |> Cell.cell1ToColor
                            { level = args.level }
                            (args.active ( x, y ) |> Just)
                , render = args.render cell
                }
            )
        |> fromPixels { width = args.width, height = args.height, size = 6 }


cell1 : { width : Int, height : Int, render : RenderFunction msg } -> String -> Svg msg
cell1 args color =
    [ { pos = ( 0, 0 ), color = color, render = args.render } ]
        |> fromPixels { width = args.width, height = args.height, size = 1 }


targetRender : { secondaryColor : String } -> RenderFunction msg
targetRender { secondaryColor } args =
    let
        ( x, y ) =
            args.pos
    in
    Svg.g
        []
        [ Svg.rect
            [ Svg.Attributes.width (args.size |> String.fromInt)
            , Svg.Attributes.height (args.size |> String.fromInt)
            , Svg.Attributes.fill args.color
            , Svg.Attributes.mask "url(#no-power)"
            , Svg.Attributes.x (x |> String.fromInt)
            , Svg.Attributes.y (y |> String.fromInt)
            ]
            []
        , Svg.circle
            [ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
            , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
            , Svg.Attributes.r (toFloat args.size * 2 / 8 |> String.fromFloat)
            , Svg.Attributes.strokeWidth (toFloat args.size / 8 |> String.fromFloat)
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke secondaryColor
            ]
            []
        ]


boxRender : { pos : ( Int, Int ), color : String, size : Int } -> Svg msg
boxRender args =
    let
        ( x, y ) =
            args.pos
    in
    Svg.rect
        [ Svg.Attributes.x (x |> String.fromInt)
        , Svg.Attributes.y (y |> String.fromInt)
        , Svg.Attributes.width (args.size |> String.fromInt)
        , Svg.Attributes.height (args.size |> String.fromInt)
        , Svg.Attributes.fill args.color
        ]
        []


connectionRender : { pos : ( Int, Int ), color : String, size : Int } -> Svg msg
connectionRender args =
    let
        ( x, y ) =
            args.pos
    in
    Svg.circle
        [ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
        , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
        , Svg.Attributes.r (toFloat args.size / 2 |> String.fromFloat)
        , Svg.Attributes.fill args.color
        ]
        []


fromPixels : { width : Int, height : Int, size : Int } -> List { pos : ( Int, Int ), color : String, render : RenderFunction msg } -> Svg msg
fromPixels args pixels =
    let
        canvasSize =
            120

        pixelSize =
            canvasSize // args.size
    in
    pixels
        |> List.map
            (\{ pos, color, render } ->
                let
                    ( x, y ) =
                        pos
                in
                render
                    { pos = ( x * pixelSize, y * pixelSize )
                    , size = pixelSize
                    , color = color
                    }
            )
        |> Svg.svg
            [ Svg.Attributes.width (String.fromInt args.width)
            , Svg.Attributes.height (String.fromInt args.height)
            , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt canvasSize ++ " " ++ String.fromInt canvasSize)
            ]
