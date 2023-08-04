module View.Svg exposing (..)

import Cell exposing (Cell)
import Dict exposing (Dict)
import Level exposing (Level)
import RelativePos exposing (RelativePos)
import Svg exposing (Svg)
import Svg.Attributes


type alias RenderFunction msg =
    { pos : ( Int, Int ), color : String, size : Int } -> Svg msg


tile :
    { cellSize : Int
    , active : ( Int, Int ) -> { originId : Maybe Int }
    , render : Cell -> RenderFunction msg
    , level : Level
    }
    -> Dict RelativePos Cell
    -> Svg msg
tile args dict =
    dict
        |> Dict.toList
        |> List.map
            (\( ( ( x, y ), _ ), cell ) ->
                { pos = ( x + 1, y + 1 )
                , color =
                    cell
                        |> Cell.toColor
                            { level = args.level
                            }
                            (args.active ( x, y ) |> Just)
                , render = args.render cell
                }
            )
        |> fromPixels { cellSize = args.cellSize, size = 6 }


singleCell : { cellSize : Int, render : RenderFunction msg } -> String -> Svg msg
singleCell args color =
    [ { pos = ( 0, 0 ), color = color, render = args.render } ]
        |> fromPixels { cellSize = args.cellSize, size = 1 }


fromPixels : { cellSize : Int, size : Int } -> List { pos : ( Int, Int ), color : String, render : RenderFunction msg } -> Svg msg
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
            [ Svg.Attributes.width (String.fromInt args.cellSize)
            , Svg.Attributes.height (String.fromInt args.cellSize)
            , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt canvasSize ++ " " ++ String.fromInt canvasSize)
            ]
