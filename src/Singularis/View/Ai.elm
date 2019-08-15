module Singularis.View.Ai exposing (view)

import Circle2d exposing (Circle2d)
import Color
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Random exposing (Generator, Seed)
import Singularis.View exposing (svgSize)
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as Types
import Vector2d exposing (Vector2d)


polygon :
    { information : Int, scale : Float, time : Float, symmetry : Float, base : Int }
    -> ( Float, Float )
    -> Generator (List Polygon2d) --(List LineSegment2d)
polygon { information, scale, time, symmetry, base } ( x, y ) =
    let
        n : Int
        n =
            information
                |> toFloat
                |> logBase 2
                |> round

        origin : Vector2d
        origin =
            Vector2d.fromComponents ( x, y )

        {- (symmetry+n-symmetry*n)*base -}
        amount : Float
        amount =
            n
                |> toFloat
                |> (*) -symmetry
                |> (+) (toFloat <| n)
                |> (+) symmetry
                |> (*) (toFloat <| base)

        angle : Float
        angle =
            2 * pi / amount

        pointGenerator : Generator Point2d
        pointGenerator =
            Random.map2
                (\i offset ->
                    Point2d.fromCoordinates
                        ( scale * sin (time + offset)
                        , 0
                        )
                        |> Point2d.rotateAround Point2d.origin (angle * toFloat i)
                        |> Point2d.translateBy origin
                )
                (Random.int 0 <| round <| amount - 1)
                (Random.float 0 <| 2 * pi - (pi * symmetry * 3 / 2))
    in
    Random.list
        n
        (Random.list base pointGenerator |> Random.map Polygon2d.singleLoop)


view : Float -> { time : Float, seed : Seed, symmetry : Float, base : Int } -> Int -> Html msg
view scale { time, seed, symmetry, base } information =
    let
        size : Float
        size =
            svgSize * scale

        circle : Circle2d
        circle =
            Point2d.fromCoordinates ( size / 32, 0 )
                |> Point2d.rotateAround Point2d.origin (time * 2)
                |> Point2d.translateBy (Vector2d.fromComponents ( size / 2, size / 2 ))
                |> Circle2d.withRadius (size / 16 + size / 64 * (sin <| time * 5))
    in
    Random.step
        (polygon
            { information = information
            , base = base
            , time = time
            , scale = size / 2
            , symmetry = symmetry
            }
            ( size / 2, size / 2 )
        )
        seed
        |> Tuple.first
        |> List.map
            (Svg.polygon2d
                [ Attributes.stroke Color.black
                , Attributes.noFill
                ]
            )
        |> (::) (Svg.circle2d [] circle)
        |> Svg.svg
            [ Attributes.width <| Types.px <| size
            , Attributes.height <| Types.px <| size
            ]
