module OracleCards.Image exposing (view)

import Angle
import Circle2d
import Geometry.Svg as Svg
import OracleCards.Card as Card exposing (Card(..))
import OracleCards.View as View
import Pixels
import Point2d
import Polygon2d
import Polyline2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes


bigRadius : Float
bigRadius =
    --View.radius * 5 / 4
    2 * View.radius / sqrt 2


viewElement : Int -> List (Svg msg)
viewElement n =
    case n of
        1 ->
            --Earth
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| View.green
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| View.green
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius / 4))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius / 4))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]

        2 ->
            --Fire
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| View.red
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| View.red
                    ]
            ]

        3 ->
            --Air
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| View.yellow
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (View.width / 2) (View.height / 2 + (bigRadius / 2) - (View.relative <| 1))
                , Point2d.pixels (View.width / 2) (View.height / 2 - (bigRadius / 2) - (View.relative <| 1))
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        4 ->
            --Water
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| View.blue
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels 0 (View.height / 2)
                , Point2d.pixels View.width (View.height / 2)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (View.width / 2) (View.height / 2 + (bigRadius / 2))
                , Point2d.pixels (View.width / 2) (View.height / 2 - (bigRadius / 2))
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        _ ->
            []


viewPlanet : Int -> List (Svg msg)
viewPlanet n =
    case n of
        1 ->
            --Wissen
            [ View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - bigRadius - View.radius / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (View.width / 2) (View.height / 2)
                , Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius - View.relative 0.5)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        2 ->
            --Venus
            [ View.regularPolygon
                { n = 3, scale = View.radius * 2 + View.relative 1, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 3, scale = View.radius, standing = False }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        3 ->
            --Erde
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 4
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2)) (Angle.radians <| pi * toFloat r / 2)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| "black"
                                        ]
                            )
                   )

        4 ->
            --Mars
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        5 ->
            --Jupiter
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 8
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2))
                                            (Angle.radians <| pi * 2 * toFloat (1 + r) / 8)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| "black"
                                        ]
                            )
                   )

        6 ->
            --Saturn
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 + bigRadius) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 + bigRadius * 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]

        7 ->
            -- Uranus
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 4
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2))
                                            (Angle.radians <| pi * 2 * toFloat (1 + r) / 4)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| "black"
                                        ]
                            )
                   )

        8 ->
            --Neptun
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (View.width / 2) (View.height / 2)
                , Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        _ ->
            []



{--viewEmotion : Int -> List (Svg msg)
viewEmotion n =
    case n of
        1 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 5
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2))
                                            (Angle.radians <| pi + pi * 2 * toFloat (1 + r) / 8)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| View.green
                                        ]
                            )
                   )

        2 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 5
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2))
                                            (Angle.radians <| pi * 2 * toFloat (1 + r) / 8)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| View.blue
                                        ]
                            )
                   )

        3 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 - bigRadius * 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 + bigRadius * 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 + bigRadius) (View.height / 2 - bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 - bigRadius) (View.height / 2 - bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]

        4 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| View.red
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 - bigRadius * 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| View.red
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 + bigRadius * 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| View.red
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 + bigRadius) (View.height / 2 + bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| View.red
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 - bigRadius) (View.height / 2 + bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| View.red
                    ]
            ]

        _ ->
            []--}


viewTrump : Int -> List (Svg msg)
viewTrump n =
    case n of
        1 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        2 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            ]

        3 ->
            []

        4 ->
            []

        5 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]

        6 ->
            []

        7 ->
            []

        8 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - bigRadius - View.radius / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius - View.relative 1 / 2)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        9 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2 + bigRadius) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2 + bigRadius * 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            ]

        10 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2 + bigRadius + View.radius / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 1 / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2 + bigRadius) (View.height / 2)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        11 ->
            []

        12 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius * 1 / 2, standing = True }
                ( View.width / 2, View.height / 2 - View.radius / (2 * sqrt 2) - View.relative 1 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (View.height / 2)
                , Point2d.pixels View.width (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        13 ->
            []

        14 ->
            []

        15 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - bigRadius - View.radius / (2 * sqrt 2) )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        16 ->
            []

        17 ->
            []

        18 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        19 ->
            []

        20 ->
            []

        21 ->
            []

        _ ->
            []


viewBack : List (Svg msg)
viewBack =
    [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
        (Pixels.pixels <| View.radius)
        |> Svg.circle2d
            [ Attributes.stroke <| "white"
            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
            , Attributes.fill <| "none"
            ]
    ]


viewVirtue : Int -> List (Svg msg)
viewVirtue n =
    case n of
        1 ->
            --Mitgefühl
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2 - View.radius + View.relative 1 / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2 + View.radius - View.relative 1 / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 4) (View.height / 2 - View.radius - View.relative 1)
                , Point2d.pixels (View.width * 3 / 4) (View.height / 2 - View.radius - View.relative 1)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        2 ->
            --Freundlichkeit
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 5
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2)) (Angle.radians <| pi * toFloat (5 + r) / 4)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r + 1 |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                "black"
                                        ]
                            )
                   )

        3 ->
            --Offenheit
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            ]

        4 ->
            --Vergebung
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "white"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (View.height / 2)
                , Point2d.pixels (View.width / 4) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 4) ((View.height / 2) - View.relative 2)
                , Point2d.pixels (View.width / 4) ((View.height / 2) + View.relative 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        5 ->
            --Geduld
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2 + bigRadius + View.radius / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2 + bigRadius + View.relative (1 / 2)) (View.height / 2)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        6 ->
            --True
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 3, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - bigRadius - View.radius / (2 * sqrt 2) )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius - (View.relative <| 0.5))
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        7 ->
            --Selbstbeherrschung
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 8
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2)) (Angle.radians <| pi * toFloat r / 4)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                "black"
                                        ]
                            )
                   )

        8 ->
            --Ausdauer
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (View.height / 2)
                , Point2d.pixels View.width (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        9 ->
            --Selbsterkenntnis
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            ]

        10 ->
            --Autentizität
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 3
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2))
                                            (Angle.radians <| pi * 2 * toFloat (1 + r) / 3)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                "black"
                                        ]
                            )
                   )

        11 ->
            --Ehrlichkeit
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2 + bigRadius) (View.height / 2)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        12 ->
            --Mäßigkeit
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 4
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius / 4)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2)) (Angle.radians <| pi * toFloat r / 2)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| Card.color <| Virtue n
                                        ]
                            )
                   )

        13 ->
            --Humor
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]
                ++ (List.range 1 5
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2)) (Angle.radians <| pi * toFloat (5 + r) / 4)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r + 1 |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                "black"
                                        ]
                            )
                   )

        14 ->
            --Hoffnung
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2 + bigRadius) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2 + bigRadius * 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            ]

        15 ->
            --Mut
            [ View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - (View.height - View.padding) / 4 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (View.height / 2)
                , Point2d.pixels View.width (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        16 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2 + bigRadius + View.radius / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2 + bigRadius + View.relative (1 / 2)) (View.height / 2)
                , Point2d.pixels (View.width / 2 - bigRadius) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        _ ->
            []


view : Card -> List (Svg msg)
view card =
    case card of
        Trump n ->
            viewTrump n

        Element n ->
            viewElement n

        Planet n ->
            viewPlanet n

        Joker ->
            [ {--Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]--}
              View.regularPolygon
                { n = 3, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        Binary n ->
            case n of
                0 ->
                    [ View.regularPolygon
                        { n = 3, scale = View.radius, standing = True }
                        ( View.width / 2, View.height / 2 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "black"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                            , Attributes.fill "none"
                            ]
                    , View.regularPolygon
                        { n = 4, scale = View.radius * 2, standing = True }
                        ( View.width / 2, View.height / 2 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "black"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                            , Attributes.fill "none"
                            ]
                    ]

                1 ->
                    [ View.regularPolygon
                        { n = 3, scale = View.radius, standing = True }
                        ( View.width / 2, View.height / 2 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                            , Attributes.fill "none"
                            ]
                    , View.regularPolygon
                        { n = 3, scale = View.radius * 1 / 2, standing = True }
                        ( View.width / 2, View.height / 2 - bigRadius - View.radius / 2 )
                        |> Polygon2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2)) (Angle.radians <| pi / 2)
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                            , Attributes.fill "none"
                            ]
                    , Polygon2d.singleLoop
                        [ Point2d.pixels (View.width / 2) (View.height / 2)
                        , Point2d.pixels (View.width / 2 + bigRadius + View.relative 1) (View.height / 2)
                        ]
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                            ]
                    ]

                _ ->
                    []

        Virtue n ->
            viewVirtue n

        Back ->
            viewBack
