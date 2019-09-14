module Generative.Shape exposing (Point, Shape, Surface(..), fromPoints, rectangle, regular, rotateBy, translateBy, withColor, withSurface)

import BoundingBox2d
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Generative.Core as Core
import Generative.Distribution exposing (Distribution)
import Point2d exposing (Point2d)
import Polygon2d
import Random exposing (Generator)
import Rectangle2d
import Vector2d exposing (Vector2d)


{-| A Surface is defined as a generator for shapes within the rectangle (0,0) to (1,1)
The density specifes the amount of shapes per pixel (so it should be a number between 0 and 1).
-}
type Surface
    = Empty
    | Filled
    | Textured
        { density : Float
        , distribution : ( Distribution, Distribution )
        , shapes : Point -> Generator Shape
        , border : Bool
        }


type alias Point =
    ( Float, Float )



--| Circle Circle2d


type alias Shape =
    Core.Shape Surface Color


withSurface : Surface -> Shape -> Shape
withSurface surface (Core.Shape p _ c) =
    Core.Shape p surface c


withColor : Color -> Shape -> Shape
withColor c (Core.Shape p s _) =
    Core.Shape p s c


rectangle : Point -> Point -> Shape
rectangle p1 p2 =
    Core.Shape
        (Rectangle2d.from
            (Point2d.fromCoordinates p1)
            (Point2d.fromCoordinates p2)
            |> Rectangle2d.toPolygon
            |> Core.Polygon
        )
        Empty
        Color.black


regular : Int -> Float -> Point -> Shape
regular n size p =
    let
        origin : Vector2d
        origin =
            Vector2d.fromComponents p

        angle : Float
        angle =
            2 * pi / toFloat n
    in
    Core.Shape
        (Point2d.fromCoordinates ( size, 0 )
            |> List.repeat n
            |> List.indexedMap
                (\i -> Point2d.rotateAround Point2d.origin <| pi / 2 + angle * toFloat i)
            |> Polygon2d.singleLoop
            |> Polygon2d.translateBy origin
            |> Core.Polygon
        )
        Empty
        Color.black


fromPoints : List Point -> Shape
fromPoints list =
    Core.Shape
        (list
            |> List.map Point2d.fromCoordinates
            |> Polygon2d.singleLoop
            |> Core.Polygon
        )
        Empty
        Color.black


rotateBy : Float -> Shape -> Shape
rotateBy angle (Core.Shape shapeType s c) =
    case shapeType of
        Core.Polygon polygon ->
            let
                center : Point2d
                center =
                    polygon
                        |> Polygon2d.boundingBox
                        |> Maybe.map BoundingBox2d.centerPoint
                        |> Maybe.withDefault Point2d.origin
            in
            Core.Shape
                (polygon
                    |> Polygon2d.rotateAround center angle
                    |> Core.Polygon
                )
                s
                c


translateBy : Point -> Shape -> Shape
translateBy p (Core.Shape shapeType s c) =
    Core.Shape
        (case shapeType of
            Core.Polygon polygon ->
                polygon
                    |> Polygon2d.translateBy (Vector2d.fromComponents p)
                    |> Core.Polygon
        )
        s
        c
