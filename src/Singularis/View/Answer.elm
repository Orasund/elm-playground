module Singularis.View.Answer exposing (view)

import Array exposing (Array(..))
import Circle2d exposing (Circle2d)
import Color
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Singularis.Data.Answer as Answer exposing (Answer, AnswerType(..))
import Singularis.View exposing (svgSize)
import Singularis.View.Polygon as Polygon
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Types as Types


size : Float
size =
    svgSize * 1


center : ( Float, Float )
center =
    ( size / 2, size / 2 )


hexagon : Polygon2d
hexagon =
    Polygon.regular { n = 6, scale = size / 2, standing = False } center


circle : Circle2d
circle =
    center
        |> Point2d.fromCoordinates
        |> Circle2d.withRadius (size / 2)


points : Array Point2d
points =
    hexagon
        |> Polygon2d.outerLoop
        |> Array.fromList


getPoint : Int -> Point2d
getPoint i =
    points
        |> Array.get i
        |> Maybe.withDefault Point2d.origin


p0 : Point2d
p0 =
    getPoint 0


p1 : Point2d
p1 =
    getPoint 1


p2 : Point2d
p2 =
    getPoint 2


p3 : Point2d
p3 =
    getPoint 3


p4 : Point2d
p4 =
    getPoint 4


p5 : Point2d
p5 =
    getPoint 5


view : Answer -> Html msg
view  =
    Answer.getAnswerImage >>
    (\( maybeA, line ) ->
    Svg.svg
        [ Attributes.width <| Types.px <| size
        , Attributes.height <| Types.px <| size
        ]
    <|
        case maybeA of
            Nothing ->
                List.concat
                    [ circle
                        |> Svg.circle2d
                            [ Attributes.stroke Color.black
                            , Attributes.noFill
                            ]
                        |> List.singleton
                    , if line then
                        LineSegment2d.fromEndpoints ( p0, p3 )
                            |> Svg.lineSegment2d
                                [ Attributes.stroke Color.black
                                , Attributes.noFill
                                ]
                            |> List.singleton

                      else
                        []
                    ]

            Just { cross, answerType } ->
                (List.concat <|
                    [ hexagon |> Polygon2d.edges
                    , if line then
                        [ LineSegment2d.fromEndpoints ( p0, p3 )
                        ]

                      else
                        []
                    , if cross then
                        [ LineSegment2d.fromEndpoints ( p1, p4 )
                        , LineSegment2d.fromEndpoints ( p2, p5 )
                        ]

                      else
                        []
                    , let
                        verticalTop : List LineSegment2d
                        verticalTop =
                            [ LineSegment2d.fromEndpoints ( p2, p4 )
                            ]

                        verticalBottom : List LineSegment2d
                        verticalBottom =
                            [ LineSegment2d.fromEndpoints ( p1, p5 )
                            ]

                        single : List LineSegment2d
                        single =
                            [ LineSegment2d.fromEndpoints ( p3, p1 )
                            , LineSegment2d.fromEndpoints ( p3, p5 )
                            ]

                        double : List LineSegment2d
                        double =
                            [ LineSegment2d.fromEndpoints ( p3, p1 )
                            , LineSegment2d.fromEndpoints ( p3, p5 )
                            , LineSegment2d.fromEndpoints ( p0, p2 )
                            , LineSegment2d.fromEndpoints ( p0, p4 )
                            ]
                      in
                      case answerType of
                        Empty ->
                            []

                        Vertical ->
                            verticalTop

                        VerticalDouble ->
                            verticalTop
                                |> List.append verticalBottom

                        Single ->
                            single

                        SingleAndVerticalTop ->
                            single
                                |> List.append verticalTop

                        SingleAndVerticalBottom ->
                            single
                                |> List.append verticalBottom

                        SingleAndVerticalBoth ->
                            single
                                |> List.append verticalTop
                                |> List.append verticalBottom

                        Double ->
                            double

                        DoubleAndVertical ->
                            double
                                |> List.append verticalTop

                        DoubleAndVerticalBoth ->
                            double
                                |> List.append verticalTop
                                |> List.append verticalBottom
                    ]
                )
                    |> List.map
                        (Svg.lineSegment2d
                            [ Attributes.stroke Color.black
                            , Attributes.noFill
                            ]
                        )

    )
    