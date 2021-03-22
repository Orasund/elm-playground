module OracleCards.View.RegularPolygon exposing (view)

import Angle
import Pixels exposing (Pixels)
import Point2d
import Polygon2d exposing (Polygon2d)
import Vector2d exposing (Vector2d)


view : { n : Int, scale : Float, standing : Bool } -> ( Float, Float ) -> Polygon2d Pixels coordinates
view { n, scale, standing } ( x, y ) =
    let
        origin : Vector2d Pixels coordinates
        origin =
            Vector2d.pixels x y

        angle : Float
        angle =
            2 * pi / toFloat n
    in
    Point2d.pixels scale 0
        |> List.repeat n
        |> List.indexedMap
            (\i -> Point2d.rotateAround Point2d.origin <| Angle.radians <| pi / 2 + angle * toFloat i)
        |> Polygon2d.singleLoop
        |> (if standing then
                Polygon2d.rotateAround Point2d.origin <| Angle.radians <| angle * 1 / 2

            else
                identity
           )
        |> Polygon2d.translateBy origin
