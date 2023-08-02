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
    , active : ( Int, Int ) -> Maybe Int
    , render : Cell -> RenderFunction msg
    , level : Level
    , connectedPathIds : List Int
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
                        |> Cell.toColor
                            { level = args.level
                            , amount = args.active ( x, y ) |> Maybe.withDefault 0
                            , connectedPathIds = args.connectedPathIds
                            }
                            (args.active ( x, y ) /= Nothing |> Just)
                , render = args.render cell
                }
            )
        |> fromPixels { width = args.width, height = args.height, size = 6 }


cell1 : { width : Int, height : Int, render : RenderFunction msg } -> String -> Svg msg
cell1 args color =
    [ { pos = ( 0, 0 ), color = color, render = args.render } ]
        |> fromPixels { width = args.width, height = args.height, size = 1 }


targetRender : { secondaryColor : String, variant : Int, small : Bool, fill : Bool } -> RenderFunction msg
targetRender { secondaryColor, variant, small, fill } args =
    let
        ( x, y ) =
            args.pos

        size =
            (if small then
                toFloat args.size / 8

             else
                toFloat args.size / 4
            )
                |> (\f ->
                        if fill then
                            f * 1.25

                        else
                            f * 1
                   )

        baseattrs =
            if fill then
                [ Svg.Attributes.fill secondaryColor ]

            else
                [ Svg.Attributes.strokeWidth
                    (size / 2 |> String.fromFloat)
                , Svg.Attributes.stroke secondaryColor
                , Svg.Attributes.fill "none"
                ]
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
        , case variant of
            2 ->
                Svg.path
                    (Svg.Attributes.d
                        (("M "
                            ++ (toFloat x + toFloat args.size / 2 |> String.fromFloat)
                            ++ " "
                            ++ (toFloat y + toFloat args.size / 2 - size |> String.fromFloat)
                            ++ ", "
                         )
                            ++ ("l " ++ (size |> String.fromFloat) ++ " " ++ (size * 2 |> String.fromFloat) ++ ", ")
                            ++ ("l " ++ (-size * 2 |> String.fromFloat) ++ " 0")
                            ++ "Z"
                        )
                        :: baseattrs
                    )
                    []

            1 ->
                Svg.rect
                    ([ Svg.Attributes.x (toFloat x + toFloat args.size / 2 - size |> String.fromFloat)
                     , Svg.Attributes.y (toFloat y + toFloat args.size / 2 - size |> String.fromFloat)
                     , Svg.Attributes.width (size * 2 |> String.fromFloat)
                     , Svg.Attributes.height (size * 2 |> String.fromFloat)
                     ]
                        ++ baseattrs
                    )
                    []

            _ ->
                Svg.circle
                    ([ Svg.Attributes.cx (toFloat x + toFloat args.size / 2 |> String.fromFloat)
                     , Svg.Attributes.cy (toFloat y + toFloat args.size / 2 |> String.fromFloat)
                     , Svg.Attributes.r (size |> String.fromFloat)
                     ]
                        ++ baseattrs
                    )
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
