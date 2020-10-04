module Sprawlopolis.Main exposing (main)

import Angle exposing (Angle)
import Array
import Circle2d exposing (Circle2d)
import Element
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Length)
import List.Extra as List
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d
import Rectangle2d exposing (Rectangle2d)
import Sprawlopolis.Card as Card
import Sprawlopolis.Color as Color exposing (Color(..))
import Sprawlopolis.Pattern as Pattern
import Sprawlopolis.View as View
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d, cross)


main : Html msg
main =
    let
        colors =
            [ Y, B, G, R ]
    in
    (colors
        |> List.permutations
        |> List.map
            (\card ->
                Card.normalView card
                    ++ (Polyline2d.fromVertices
                            [ Point2d.pixels 0 0
                            , Point2d.pixels View.width 0
                            , Point2d.pixels View.width View.height
                            , Point2d.pixels 0 View.height
                            ]
                            |> Svg.polyline2d
                                [ Attributes.stroke <| "black"
                                , Attributes.strokeWidth <| String.fromFloat <| View.relative 3
                                , Attributes.fill <| "transparent"
                                ]
                            |> List.singleton
                       )
            )
    )
        ++ (colors
                |> List.permutations
                |> List.map
                    (\card ->
                        Pattern.patterns
                            ++ Card.view card
                            ++ (Polyline2d.fromVertices
                                    [ Point2d.pixels 0 0
                                    , Point2d.pixels View.width 0
                                    , Point2d.pixels View.width View.height
                                    , Point2d.pixels 0 View.height
                                    ]
                                    |> Svg.polyline2d
                                        [ Attributes.stroke <| "black"
                                        , Attributes.strokeWidth <| String.fromFloat <| View.relative 3
                                        , Attributes.fill <| "transparent"
                                        ]
                                    |> List.singleton
                               )
                    )
           )
        |> List.map
            (Svg.svg
                [ Attributes.width <| String.fromFloat <| View.zoom * View.width
                , Attributes.height <| String.fromFloat <| View.zoom * View.height
                , Attributes.viewBox <|
                    "0 0 "
                        ++ String.fromFloat View.width
                        ++ " "
                        ++ String.fromFloat View.height
                ]
            )
        |> Html.div []



{--
--}
