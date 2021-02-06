module OracleCards.Sigil exposing (view)

import Angle
import Arc2d
import Binary
import Circle2d exposing (radius)
import Geometry.Svg as Svg
import LineSegment2d
import OracleCards.View as View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


radius : Float
radius =
    View.relative <| 1 / 2


strokeWidth : Float
strokeWidth =
    View.relative <| 1 / 8


type Direction
    = Up
    | Down


type Shape
    = Circle
    | Start
    | End
    | SemiCircle
    | Line
    | Loop
    | DoubleLoop
    | LineLoop
    | LineDoubleLoop
    | Spiral
    | Singleton


shapeToSvg : String -> Vector2d Pixels coord -> Direction -> Shape -> ( Point2d Pixels coord -> List (Svg msg), Vector2d Pixels coord, Direction )
shapeToSvg color p direction shape =
    case ( direction, shape ) of
        ( d, Singleton ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ (point
                    |> Point2d.translateBy (Vector2d.pixels -radius 0)
                  )
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels -(radius / 2) 0)
                    |> LineSegment2d.from (point |> Point2d.translateBy (Vector2d.pixels (radius / 2) 0))
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , (point
                    |> Point2d.translateBy (Vector2d.pixels radius 0)
                  )
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p
            , d
            )

        ( Up, Spiral ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels -(radius * 4) 0)
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels -(radius * 4) 0)
                            |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0)
                        )
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 -(radius / 2))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels -(radius * 4) 0)
            , Up
            )

        ( Down, Spiral ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels (radius * 4) 0)
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels (radius * 4) 0)
                            |> Point2d.translateBy (Vector2d.pixels -(radius * 2) 0)
                        )
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , (point |> Point2d.translateBy (Vector2d.pixels 0 (radius / 2)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels (radius * 4) 0)
            , Down
            )

        ( Up, Start ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0))
                        (Angle.degrees 90)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , (point |> Point2d.translateBy (Vector2d.pixels (radius * 2.5) -(radius * 2)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p
            , Up
            )

        ( Down, Start ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                (point |> Point2d.translateBy (Vector2d.pixels 0 (radius / 2)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                    |> List.singleton
            , p
            , Down
            )

        ( Up, End ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels radius 0)
                    |> LineSegment2d.from point
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , (point
                    |> Point2d.translateBy (Vector2d.pixels radius 0)
                    |> Point2d.translateBy (Vector2d.pixels (radius / 2) 0)
                  )
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p
            , Up
            )

        ( Down, End ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels 0 radius)
                    |> LineSegment2d.from point
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 radius)
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels radius radius))
                        (Angle.degrees -90)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , (point |> Point2d.translateBy (Vector2d.pixels (radius * 1.5) (radius * 2)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p
            , Down
            )

        ( Up, Circle ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                (point |> Point2d.translateBy (Vector2d.pixels 0 -(radius / 2)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                    |> List.singleton
            , p
            , Up
            )

        ( Down, Circle ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                (point |> Point2d.translateBy (Vector2d.pixels 0 (radius / 2)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                    |> List.singleton
            , p
            , Down
            )

        ( Up, SemiCircle ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                point
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels radius 0))
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                    |> List.singleton
            , p |> Vector2d.plus (Vector2d.pixels (radius * 2) 0)
            , Up
            )

        ( Down, SemiCircle ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                point
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels radius 0))
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                    |> List.singleton
            , p |> Vector2d.plus (Vector2d.pixels (radius * 2) 0)
            , Down
            )

        ( Up, Loop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels (radius * 1.5) 0))
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , (point |> Point2d.translateBy (Vector2d.pixels (radius * 1.5) (2 * radius)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels (radius * 3) 0)
            , Up
            )

        ( Down, Loop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels (radius * 1.5) 0))
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , (point |> Point2d.translateBy (Vector2d.pixels (radius * 1.5) -(2 * radius)))
                    |> Circle2d.withRadius (Pixels.pixels (radius / 2))
                    |> Svg.circle2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels (radius * 3) 0)
            , Down
            )

        ( Up, DoubleLoop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4))
                    |> LineSegment2d.from point
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels -(radius / 2) (radius * 4)))
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels -radius (radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels radius (radius * 4)))
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels (radius * 4) (radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels (radius * 2.5) (radius * 4)))
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels (radius * 2) (radius * 4))
                    |> LineSegment2d.from (point |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0))
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels (radius * 2) 0)
            , Up
            )

        ( Down, DoubleLoop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels 0 -(radius * 4))
                    |> LineSegment2d.from point
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 -(radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels -(radius / 2) -(radius * 4)))
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels -radius -(radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels radius -(radius * 4)))
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels (radius * 4) -(radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels (radius * 2.5) -(radius * 4)))
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels (radius * 2) -(radius * 4))
                    |> LineSegment2d.from (point |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0))
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels (radius * 2) 0)
            , Down
            )

        ( Up, Line ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                point
                    |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4))
                    |> LineSegment2d.from point
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                    |> List.singleton
            , p |> Vector2d.plus (Vector2d.pixels 0 (radius * 4))
            , Down
            )

        ( Down, Line ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                (point |> Point2d.translateBy (Vector2d.pixels 0 (-radius * 4)))
                    |> LineSegment2d.from point
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                    |> List.singleton
            , p |> Vector2d.plus (Vector2d.pixels 0 (-radius * 4))
            , Up
            )

        ( Up, LineLoop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4))
                    |> LineSegment2d.from (point |> Point2d.translateBy (Vector2d.pixels 0 0))
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4))
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4))
                            |> Point2d.translateBy (Vector2d.pixels (-radius / 2) 0)
                        )
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels -radius (radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4)))
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels radius (radius * 4))
            , Down
            )

        ( Down, LineLoop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Point2d.translateBy (Vector2d.pixels 0 -(radius * 4))
                    |> LineSegment2d.from (point |> Point2d.translateBy (Vector2d.pixels 0 0))
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 -(radius * 4))
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels 0 -(radius * 4))
                            |> Point2d.translateBy (Vector2d.pixels (-radius / 2) 0)
                        )
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels -radius -(radius * 4))
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels 0 -(radius * 4)))
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels radius (-radius * 4))
            , Up
            )

        ( Down, LineDoubleLoop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels radius 0))
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0)
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0)
                            |> Point2d.translateBy (Vector2d.pixels (-radius / 2) 0)
                        )
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels radius -(radius * 4))
                    |> LineSegment2d.from (point |> Point2d.translateBy (Vector2d.pixels radius 0))
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels radius -(radius * 4))
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels radius -(radius * 4))
                            |> Point2d.translateBy (Vector2d.pixels -(radius / 2) 0)
                        )
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 -(radius * 4))
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels radius -(radius * 4))
                        )
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels (radius * 2) -(radius * 4))
            , Up
            )

        ( Up, LineDoubleLoop ) ->
            ( \offset ->
                let
                    point =
                        offset |> Point2d.translateBy p
                in
                [ point
                    |> Arc2d.sweptAround
                        (point |> Point2d.translateBy (Vector2d.pixels radius 0))
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0)
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels (radius * 2) 0)
                            |> Point2d.translateBy (Vector2d.pixels (-radius / 2) 0)
                        )
                        (Angle.degrees -180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels radius (radius * 4))
                    |> LineSegment2d.from (point |> Point2d.translateBy (Vector2d.pixels radius 0))
                    |> Svg.lineSegment2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels radius (radius * 4))
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels radius (radius * 4))
                            |> Point2d.translateBy (Vector2d.pixels (-radius / 2) 0)
                        )
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                , point
                    |> Point2d.translateBy (Vector2d.pixels 0 (radius * 4))
                    |> Arc2d.sweptAround
                        (point
                            |> Point2d.translateBy (Vector2d.pixels radius (radius * 4))
                        )
                        (Angle.degrees 180)
                    |> Svg.arc2d
                        [ Attributes.stroke color
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
                ]
            , p |> Vector2d.plus (Vector2d.pixels (radius * 2) (radius * 4))
            , Down
            )


shapeListToSvg : String -> Vector2d Pixels coord -> Direction -> List Shape -> ( Point2d Pixels coord -> List (Svg msg), Vector2d Pixels coord, Direction )
shapeListToSvg color p d =
    List.foldl
        (\a ( out, point, direction ) ->
            let
                ( svg, newPoint, newDirection ) =
                    a |> shapeToSvg color point direction
            in
            ( \offset -> svg offset ++ out offset
            , newPoint
            , newDirection
            )
        )
        ( \offset -> [], p, d )


boolListToShapeList : List Bool -> ( Direction, List Shape )
boolListToShapeList l =
    let
        rec rem =
            case rem of
                [] ->
                    []

                [ _ ] ->
                    [ End ]

                [ True, False ] ->
                    [ Line, End ]

                [ False, True ] ->
                    [ Line, End ]

                True :: False :: ((True :: _) as tail) ->
                    Loop :: rec tail

                False :: True :: ((False :: _) as tail) ->
                    Loop :: rec tail

                True :: ((True :: _) as tail) ->
                    SemiCircle :: rec tail

                False :: ((False :: _) as tail) ->
                    SemiCircle :: rec tail

                _ :: ((_ :: _) as tail) ->
                    Line :: rec tail
    in
    ( case l |> List.head of
        Just True ->
            Up

        Just False ->
            Down

        Nothing ->
            Up
    , case l of
        [] ->
            [ Singleton ]

        [ True ] ->
            [ Spiral, End ]

        [ False ] ->
            [ Spiral, End ]

        [ True, False ] ->
            [ Start, LineLoop, End ]

        [ False, True ] ->
            [ Start, LineLoop, End ]

        [ True, False, True ] ->
            [ Start, Loop, End ]

        [ False, True, False ] ->
            [ Start, Loop, End ]

        True :: False :: ((True :: _) as tail) ->
            Start :: Loop :: rec tail

        False :: True :: ((False :: _) as tail) ->
            Start :: Loop :: rec tail

        False :: ((True :: _) as tail) ->
            Start :: Line :: rec tail

        True :: ((False :: _) as tail) ->
            Start :: Line :: rec tail

        _ ->
            Start :: rec l
    )


view : Point2d Pixels coord -> { value : Int, size : Int, color : String } -> List (Svg msg)
view point { value, size, color } =
    let
        ( direction, shapeList ) =
            value
                |> Binary.fromDecimal
                |> Binary.ensureSize size
                |> Binary.toBooleans
                |> boolListToShapeList

        ( list, dimensions, _ ) =
            shapeList
                |> shapeListToSvg color
                    Vector2d.zero
                    direction

        dim =
            dimensions
                |> Vector2d.toPixels
    in
    list
        (point
            |> Point2d.translateBy
                (case direction of
                    Up ->
                        Vector2d.pixels 0 (1 * radius)

                    Down ->
                        Vector2d.pixels 0 (5 * radius)
                )
            |> Point2d.translateBy (Vector2d.pixels (-dim.x / 2) 0)
        )
