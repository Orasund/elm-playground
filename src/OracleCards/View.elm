module OracleCards.View exposing (blackBackground, blue, green, height, padding, radius, red, regularPolygon, relative, width, zoom)

import Angle
import Pixels exposing (Pixels)
import Point2d
import Polygon2d exposing (Polygon2d)
import Vector2d exposing (Vector2d)


blackBackground : String
blackBackground =
    "black"



--"LightGray"


green : String
green =
    "#12BE52"



--"#00F600"


blue : String
blue =
    "#1E88E5"



--"#3D04F2"


red : String
red =
    "#DC2626"


zoom : Float
zoom =
    0.36



--4 * 0.63
--0.63
--0.73


width : Float
width =
    898



--52
-- 70


height : Float
height =
    1488



--86
--120


padding : Float
padding =
    12 * (3 + (4 * 2))



--relative 7


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
    (*) (factor * width / 70)



--factor


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
