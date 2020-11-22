module LinesCards.View exposing (blackBackground, blue, green, height, padding, radius, red, regularPolygon, relative, width, yellow, zoom)

import Angle
import Pixels exposing (Pixels)
import Point2d
import Polygon2d exposing (Polygon2d)
import Vector2d exposing (Vector2d)


blackBackground : String
blackBackground =
    "LightGray"


yellow : String
yellow =
    "#F0E442"


green : String
green =
    "#009E73"


blue : String
blue =
    "#56B4E9"


red : String
red =
    "#D55E00"


zoom : Float
zoom =
    4


width : Float
width =
    52


height : Float
height =
    74


padding : Float
padding =
    relative 7


radius : Float
radius =
    (width - padding) / 7



--relative 4


relative : Float -> Float
relative =
    let
        factor =
            if height > width then
                height / width

            else
                width / height
    in
    (*) factor


regularPolygon : { n : Int, scale : Float, standing : Bool } -> ( Float, Float ) -> Polygon2d Pixels coordinates
regularPolygon { n, scale, standing } ( x, y ) =
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
