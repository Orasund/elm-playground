module Generative exposing (toHtml, toSvg)

import BoundingBox2d
import Circle2d
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
toSvg (Core.Shape shapeType surface color) =
    case shapeType of
        Core.Polygon polygon ->
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

                                draw : Shape -> Generator (List (Svg msg))
                                draw ((Core.Shape shapeType2 s c) as shape) =
                                    case shapeType2 of
                                        Core.Polygon polygon2 ->
                                            if
                                                polygon2
                                                    |> Polygon2d.vertices
                                                    |> List.all
                                                        (Point2d.coordinates
                                                            >> (\point ->
                                                                    Core.contains
                                                                        (polygon
                                                                            |> Polygon2d.edges
                                                                        )
                                                                        point
                                                                        0
                                                               )
                                                        )
                                            then
                                                shape |> toSvg

                                            else
                                                Random.constant []

                                        Core.Circle circle ->
                                            Shape.regular 12
                                                (circle |> Circle2d.radius)
                                                (circle
                                                    |> Circle2d.centerPoint
                                                    |> Point2d.coordinates
                                                )
                                                |> Shape.withSurface s
                                                |> Shape.withColor c
                                                |> draw
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
                                                        >> draw
                                                    )
                                        )
                                )
                                |> Random.andThen
                                    (List.foldl
                                        (Random.map2
                                            List.append
                                        )
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

        Core.Circle circle ->
            case surface of
                Empty ->
                    circle
                        |> Svg.circle2d
                            [ Attributes.fill "none"
                            , Attributes.strokeWidth "1"
                            , Attributes.stroke <| Color.toCssString <| color
                            ]
                        |> List.singleton
                        |> Random.constant

                Filled ->
                    circle
                        |> Svg.circle2d
                            [ Attributes.fill <| Color.toCssString <| color
                            , Attributes.strokeWidth "1"
                            , Attributes.stroke <| Color.toCssString <| color
                            ]
                        |> List.singleton
                        |> Random.constant

                Textured ({ border } as texture) ->
                    Shape.regular 12
                        (circle |> Circle2d.radius)
                        (circle
                            |> Circle2d.centerPoint
                            |> Point2d.coordinates
                        )
                        |> Shape.withSurface (Textured { texture | border = False })
                        |> toSvg
                        |> (if border then
                                Random.map
                                    ((::)
                                        (circle
                                            |> Svg.circle2d
                                                [ Attributes.fill "none"
                                                , Attributes.strokeWidth "1"
                                                , Attributes.stroke <| Color.toCssString <| color
                                                ]
                                        )
                                    )

                            else
                                identity
                           )


toHtml : List (Attribute msg) -> List (Generator (List (Svg msg))) -> Generator (Html msg)
toHtml attributes =
    List.foldl (Random.map2 List.append) (Random.constant [])
        >> Random.map (Svg.svg attributes)
