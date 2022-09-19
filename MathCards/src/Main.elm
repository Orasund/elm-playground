module Main exposing (..)

import Config
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


plot : { minX : Float, maxX : Float, points : Int } -> (Float -> Float) -> List ( Float, Float )
plot args fun =
    List.range 0 (args.points + 1)
        |> List.map (\i -> args.minX + (args.maxX - args.minX) * toFloat i / toFloat (args.points + 1))
        |> List.map (\f -> ( f, fun f ))


polygon : String -> List ( Float, Float ) -> Svg msg
polygon color list =
    Svg.polygon
        [ list
            |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
            |> String.join " "
            |> Svg.Attributes.points
        , "fill:" ++ color ++ ";stroke:" ++ color ++ ";stroke-width:0.01" |> Svg.Attributes.style
        ]
        []


polyLine : List ( Float, Float ) -> Svg msg
polyLine list =
    Svg.polyline
        [ list
            |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
            |> String.join " "
            |> Svg.Attributes.points
        , Svg.Attributes.style "stroke:white;stroke-width:0.005"
        ]
        []


crosshair : List (Svg msg)
crosshair =
    [ [ ( -0.5, 0 ), ( 0.5, 0 ) ] |> polyLine
    , [ ( 0, -0.5 ), ( 0, 0.5 ) ] |> polyLine
    ]


cardFromPlot : Int -> (Float -> Float -> Float) -> List (Svg msg)
cardFromPlot n fun =
    let
        stepSize =
            Config.viewSize.width / (toFloat n + 1)

        steps =
            n

        zoom =
            3 / 4
    in
    List.range 0 steps
        |> List.map
            (\i ->
                ((-(toFloat (steps + 1) * stepSize / 2) + toFloat i * stepSize |> fun)
                    |> plot { minX = -zoom, maxX = zoom, points = 100 }
                )
                    ++ ((-(toFloat (steps + 1) * stepSize / 2) + toFloat (i + 1) * stepSize |> fun)
                            |> plot { minX = -zoom, maxX = zoom, points = 100 }
                            |> List.reverse
                       )
                    |> List.map (\( x, y ) -> ( x / zoom, -y / zoom ))
                    |> polygon
                        (if modBy 2 i == 0 then
                            "LightSalmon"

                         else
                            "Salmon"
                        )
            )


card : Int -> Html msg
card n =
    cardFromPlot n (\c x -> x ^ toFloat n + c)
        ++ crosshair
        |> Svg.svg
            [ Config.width |> Html.Attributes.width
            , Config.height |> Html.Attributes.height
            , String.fromFloat -(Config.viewSize.width / 2)
                ++ " "
                ++ String.fromFloat -(Config.viewSize.height / 2)
                ++ " "
                ++ String.fromFloat Config.viewSize.width
                ++ " "
                ++ String.fromFloat Config.viewSize.height
                |> Svg.Attributes.viewBox
            ]


main : Html msg
main =
    List.range 1 6
        |> List.map card
        |> Html.div []
