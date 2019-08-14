module Singularis.View.Polygon exposing (regular, view)

import Color
import Geometry.Svg as Svg
import Html exposing (Html)
import Point2d
import Polygon2d exposing (Polygon2d)
import Singularis.View exposing (svgSize)
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as Types
import Vector2d exposing (Vector2d)


view : Int -> Html msg
view n =
    let
        size : Float
        size =
            svgSize
    in
    Svg.svg
        [ Attributes.width <| Types.px <| size
        , Attributes.height <| Types.px <| size
        ]
    <|
        [ regular { n = n, scale = size / 2, standing = True } ( size / 2, size / 2 )
            |> Svg.polygon2d
                [ Attributes.stroke Color.black
                , Attributes.noFill
                ]
        ]


{-| Creates a regular polygon with radius 1 around a origin point.
-}
regular : { n : Int, scale : Float, standing : Bool } -> ( Float, Float ) -> Polygon2d
regular { n, scale, standing } ( x, y ) =
    let
        origin : Vector2d
        origin =
            Vector2d.fromComponents ( x, y )

        angle : Float
        angle =
            2 * pi / toFloat n
    in
    Point2d.fromCoordinates ( scale, 0 )
        |> List.repeat n
        |> List.indexedMap
            (\i -> Point2d.rotateAround Point2d.origin <| pi / 2 + angle * toFloat i)
        |> Polygon2d.singleLoop
        |> (if standing then
                Polygon2d.rotateAround Point2d.origin <| angle * 1 / 2

            else
                identity
           )
        |> Polygon2d.translateBy origin
