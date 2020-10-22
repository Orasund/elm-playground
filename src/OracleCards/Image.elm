module OracleCards.Image exposing (view)

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Circle2d exposing (Circle2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import OracleCards.Card as Card exposing (Card(..))
import OracleCards.View as View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


bigRadius =
    --View.radius * 5 / 4
    2 * View.radius / sqrt 2


viewSeason : Int -> List (Svg msg)
viewSeason n =
    case n of
        1 ->
            --Earth
            [ Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Arc2d.with
                { centerPoint = Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius * 2)
                , radius = Pixels.pixels <| bigRadius * 2
                , startAngle = Angle.radians pi
                , sweptAngle = Angle.radians pi
                }
                |> Svg.arc2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        2 ->
            --Fire
            [ Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Arc2d.with
                { centerPoint = Point2d.pixels (View.width / 2) (View.height / 2)
                , radius = Pixels.pixels <| bigRadius * 2
                , startAngle = Angle.radians pi
                , sweptAngle = Angle.radians pi
                }
                |> Svg.arc2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        3 ->
            --Air
            [ Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Arc2d.with
                { centerPoint = Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                , radius = Pixels.pixels <| bigRadius * 2
                , startAngle = Angle.radians 0
                , sweptAngle = Angle.radians pi
                }
                |> Svg.arc2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        4 ->
            --Water
            [ Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Arc2d.with
                { centerPoint = Point2d.pixels (View.width / 2) (View.height / 2)
                , radius = Pixels.pixels <| bigRadius * 2
                , startAngle = Angle.radians 0
                , sweptAngle = Angle.radians pi
                }
                |> Svg.arc2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        _ ->
            []


viewDirection : Int -> List (Svg msg)
viewDirection n =
    case n of
        1 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2)
                    |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2))
                        (Angle.radians <| pi * 2 * toFloat 1 / 4)
                )
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <|
                        "black"
                    ]
            ]

        2 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                                            "black"
                                        ]
                            )
                   )

        3 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2 - bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <|
                        "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 - bigRadius * 2) (View.height / 2 - bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <|
                        "black"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2 + bigRadius * 2) (View.height / 2 - bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <|
                        "black"
                    ]
            ]

        4 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                                            (Angle.radians <| pi / 4 + pi * 2 * toFloat (1 + r) / 4)
                                    )
                                    (Pixels.pixels <| View.relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            "black"
                                        ]
                            )
                   )

        _ ->
            []


viewAnimal : Int -> List (Svg msg)
viewAnimal n =
    case n of
        1 ->
            --unicorn
            [ View.regularPolygon
                { n = 3, scale = View.radius * 2, standing = False }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 - View.radius )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        2 ->
            --dragon
            [ View.regularPolygon
                { n = 3, scale = View.radius * 2, standing = False }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (View.width / 2) (View.height / 2 - View.radius * 2)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        3 ->
            --phonix
            [ View.regularPolygon
                { n = 3, scale = View.radius * 2, standing = False }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 - View.radius )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        4 ->
            --Turtle
            [ View.regularPolygon
                { n = 3, scale = View.radius * 2, standing = False }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 - View.radius))
                (Pixels.pixels <| View.radius)
                |> Svg.circle2d
                    [ Attributes.fill "transparent"
                    , Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        _ ->
            []


viewEmotion : Int -> List (Svg msg)
viewEmotion n =
    case n of
        1 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + View.radius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Arc2d.with
                { centerPoint = Point2d.pixels (View.width / 2) (View.height / 2)
                , radius = Pixels.pixels <| View.radius * 2
                , startAngle = Angle.radians pi
                , sweptAngle = Angle.radians <| pi
                }
                |> Svg.arc2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        2 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + View.radius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Arc2d.with
                { centerPoint = Point2d.pixels (View.width / 2) (View.height / 2 - View.radius * 2)
                , radius = Pixels.pixels <| View.radius * 2
                , startAngle = Angle.radians 0
                , sweptAngle = Angle.radians <| pi
                }
                |> Svg.arc2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        3 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + View.radius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (View.width / 2 + View.radius * 2) (View.height / 2)
                , Point2d.pixels (View.width / 2) (View.height / 2 - View.radius * 2)
                , Point2d.pixels (View.width / 2 - View.radius * 2) (View.height / 2)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        4 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + View.radius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (View.width / 2 + View.radius * 2) (View.height / 2 - View.radius * 2)
                , Point2d.pixels (View.width / 2) (View.height / 2)
                , Point2d.pixels (View.width / 2 - View.radius * 2) (View.height / 2 - View.radius * 2)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        _ ->
            []


viewTrump : Int -> List (Svg msg)
viewTrump n =
    case n of
        1 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "transparent"
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
                    , Attributes.fill "transparent"
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
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                                                Card.color <| Trump n

                                            else
                                                "black"
                                        ]
                            )
                   )

        4 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                                                Card.color <| Trump n

                                            else
                                                "black"
                                        ]
                            )
                   )

        5 ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "transparent"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            ]

        6 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2 - View.radius + View.relative 1, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2 + View.radius, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 4) (View.height / 2 - View.radius - View.relative 1)
                , Point2d.pixels (View.width * 3 / 4) (View.height / 2 - View.radius - View.relative 1)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        7 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                                        [ Attributes.fill <| Card.color <| Trump n
                                        ]
                            )
                   )

        8 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - bigRadius - View.radius / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                    , Attributes.fill "transparent"
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
                    , Attributes.fill "transparent"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2 + bigRadius + View.radius / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 1 / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill <| "transparent"
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
            [ View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - (View.height - View.padding) / 4 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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

        12 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius * 1 / 2, standing = True }
                ( View.width / 2, View.height / 2 - View.radius / (2 * sqrt 2) - View.relative 1 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , View.regularPolygon
                { n = 3, scale = View.radius * 1 / 2, standing = True }
                ( View.width / 2, View.height / 2 - bigRadius - View.radius / 2 )
                |> Polygon2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2)) (Angle.radians <| pi / 2)
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (View.width / 2) (View.height / 2)
                , Point2d.pixels (View.width / 2 + bigRadius + View.relative 1 / 2) (View.height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    ]
            ]

        14 ->
            [ View.regularPolygon
                { n = 4, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                                        [ Attributes.fill <| Card.color <| Trump n
                                        ]
                            )
                   )

        15 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius / 2, standing = True }
                ( View.width / 2, View.height / 2 - bigRadius - View.radius / (2 * sqrt 2) )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , View.regularPolygon
                { n = 4, scale = View.radius * 2, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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

        17 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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

        18 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
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
                                                Card.color <| Trump n

                                            else
                                                "black"
                                        ]
                            )
                   )

        20 ->
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

        21 ->
            [ View.regularPolygon
                { n = 3, scale = View.radius, standing = True }
                ( View.width / 2, View.height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| "black"
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2 + bigRadius * 2))
                (Pixels.pixels <| View.relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            ]

        _ ->
            []


view : Card -> List (Svg msg)
view card =
    case card of
        Trump n ->
            viewTrump n

        Emotion n ->
            viewEmotion n

        Animal n ->
            viewAnimal n

        Season n ->
            viewSeason n

        Direction n ->
            viewDirection n

        Joker ->
            [ Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                (Pixels.pixels <| View.radius * 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                    , Attributes.fill "transparent"
                    ]
            ]

        _ ->
            let
                value =
                    Card.value card

                isWhite =
                    case card of
                        Black _ ->
                            False

                        _ ->
                            True
            in
            [ case value of
                1 ->
                    Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                        (Pixels.pixels <| View.relative <| 1 / 2)
                        |> Svg.circle2d
                            [ Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]

                7 ->
                    Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height / 2))
                        (Pixels.pixels <| View.radius * 2)
                        |> Svg.circle2d
                            [ Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]

                2 ->
                    Rectangle2d.from (Point2d.pixels 0 (View.height / 2))
                        (Point2d.pixels View.width View.height)
                        |> Svg.rectangle2d
                            [ Attributes.stroke <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
                            , Attributes.fill <|
                                if isWhite then
                                    View.blackBackground

                                else
                                    "white"
                            ]

                _ ->
                    View.regularPolygon
                        { n = value, scale = View.radius * 2, standing = True }
                        ( View.width / 2, View.height / 2 )
                        |> Svg.polygon2d
                            [ Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]
            ]
