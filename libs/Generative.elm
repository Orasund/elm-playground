module Generative exposing (toHtml, toSvg)

import BoundingBox2d
import Color
import Generative.Core as Core
import Generative.Distribution exposing (Distribution)
import Generative.Shape as Shape exposing (Point, Shape, Surface(..))
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


toSvg : Shape -> Generator (List (Svg msg))
toSvg (Core.Shape polygon surface color) =
    case polygon |> Polygon2d.boundingBox of
        Nothing ->
            Random.constant []

        Just boundingBox ->
            case surface of
                Empty ->
                    polygon
                        |> Svg.polygon2d
                            [ Attributes.fill "none"
                            , Attributes.strokeWidth "1"
                            , Attributes.stroke <| Color.toCssString <| color
                            ]
                        |> List.singleton
                        |> Random.constant

                Filled ->
                    polygon
                        |> Svg.polygon2d
                            [ Attributes.fill <| Color.toCssString <| color
                            , Attributes.strokeWidth "1"
                            , Attributes.stroke <| Color.toCssString <| color
                            ]
                        |> List.singleton
                        |> Random.constant

                Textured { density, distribution, shapes, border } ->
                    let
                        ( width, height ) =
                            boundingBox |> BoundingBox2d.dimensions

                        pos : { x : Float, y : Float }
                        pos =
                            { x = boundingBox |> BoundingBox2d.minX
                            , y = boundingBox |> BoundingBox2d.minY
                            }

                        pointGenerator : Generator Point
                        pointGenerator =
                            distribution |> (\( x, y ) -> Random.pair x y)

                        amount : Int
                        amount =
                            round <| width * height * density
                    in
                    Random.list amount
                        (pointGenerator
                            |> Random.andThen
                                (\p ->
                                    p
                                        |> shapes
                                        |> Random.map
                                            (Shape.translateBy
                                                (p
                                                    |> Tuple.mapBoth
                                                        ((*) width >> (+) pos.x)
                                                        ((*) height >> (+) pos.y)
                                                )
                                                >> toSvg
                                            )
                                )
                        )
                        |> Random.andThen
                            (List.foldl
                                (Random.map2 List.append)
                                (Random.constant <|
                                    if border then
                                        polygon
                                            |> Svg.polygon2d
                                                [ Attributes.fill "none"
                                                , Attributes.strokeWidth "1"
                                                , Attributes.stroke <| Color.toCssString <| color
                                                ]
                                            |> List.singleton

                                    else
                                        []
                                )
                            )


toHtml : List (Attribute msg) -> List (Generator (List (Svg msg))) -> Generator (Html msg)
toHtml attributes =
    List.foldl (Random.map2 List.append) (Random.constant [])
        >> Random.map (Svg.svg attributes)
