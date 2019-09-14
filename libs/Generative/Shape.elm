module Generative.Shape exposing (Point, Shape, Surface(..), rectangle, rotateBy, translateBy, withColor, withSurface)

import BoundingBox2d
import Color exposing (Color)
import Generative.Core as Core
import Generative.Distribution exposing (Distribution)
import Geometry.Svg as Svg
import Html exposing (Attribute, Html)
import Location
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Random exposing (Generator)
import Random.Float as Random
import Rectangle2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d


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


type alias Shape =
    Core.Shape Polygon2d Surface Color


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
        )
        Empty
        Color.black


rotateBy : Float -> Shape -> Shape
rotateBy angle (Core.Shape polygon s c) =
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
        )
        s
        c


translateBy : Point -> Shape -> Shape
translateBy p (Core.Shape polygon s c) =
    Core.Shape
        (polygon |> Polygon2d.translateBy (Vector2d.fromComponents p))
        s
        c
