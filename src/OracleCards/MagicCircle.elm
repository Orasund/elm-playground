module OracleCards.MagicCircle exposing (..)

import Angle
import Arc2d
import Binary
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import OracleCards.Sigil as Sigil
import OracleCards.View as View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


size =
    View.relative <| 100


ringWidth =
    View.relative <| 5


main : Html msg
main =
    [ Circle2d.atPoint (Point2d.pixels (size / 2) (size / 2))
        (Pixels.pixels <| ringWidth)
        |> Svg.circle2d
            [ Attributes.fill <| "none"
            , Attributes.stroke <| "black"
            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
            ]
    ]
        {--++ ({ value = 0
            , size = 0
            , color = "black"
            , radius = View.relative <| 1 / 2
            , strokeWidth = View.relative <| 1 / 8
            , point = Point2d.pixels (size / 2) (size / 2)
            }
                |> Sigil.view
           )--}
        ++ (List.range 1 4
                |> List.concatMap
                    (\n ->
                        [ Circle2d.atPoint (Point2d.pixels (size / 2) (size / 2))
                            (Pixels.pixels <| ringWidth * (1 + 2 * toFloat n))
                            |> Svg.circle2d
                                [ Attributes.fill <| "none"
                                , Attributes.stroke <| "black"
                                , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                                ]
                        ]
                            ++ (List.range 1 (2 ^ n)
                                    |> List.concatMap
                                        (\r ->
                                            (Point2d.pixels (size / 2) (size / 2)
                                                |> Point2d.translateBy (Vector2d.pixels (ringWidth * (1 + 2 * toFloat n)) 0)
                                                |> LineSegment2d.from
                                                    (Point2d.pixels (size / 2) (size / 2)
                                                        |> Point2d.translateBy (Vector2d.pixels (ringWidth * (1 + 2 * (toFloat n - 1))) 0)
                                                    )
                                                |> LineSegment2d.rotateAround (Point2d.pixels (size / 2) (size / 2))
                                                    (Angle.radians <| (2 * pi / toFloat (2 ^ n)) * toFloat r)
                                                |> Svg.lineSegment2d
                                                    [ Attributes.stroke "black"
                                                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                                                    , Attributes.fill "none"
                                                    ]
                                            )
                                                :: ({ value = r
                                                    , size = n
                                                    , color = "black"
                                                    , radius = View.relative <| 1 / 2
                                                    , strokeWidth = View.relative <| 1 / 8
                                                    , point =
                                                        Point2d.pixels (size / 2) (size / 2)
                                                            |> Point2d.translateBy (Vector2d.pixels (ringWidth * (2 * toFloat n)) 0)
                                                            |> Point2d.rotateAround (Point2d.pixels (size / 2) (size / 2))
                                                                (Angle.radians <| (2 * pi / toFloat (2 ^ n)) * (0.5 + toFloat r))
                                                    }
                                                        |> Sigil.view
                                                   )
                                        )
                               )
                    )
           )
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| View.zoom * size) ++ "px"
            , Attributes.height <| (String.fromFloat <| View.zoom * size) ++ "px"
            , Attributes.version <| "1.1"
            , Attributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat size
                    ++ " "
                    ++ String.fromFloat size
            ]