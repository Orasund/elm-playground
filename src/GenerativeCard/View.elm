module GenerativeCard.View exposing (line)

import Color exposing (Color)
import Geometry.Svg as Svg
import LineSegment2d exposing (LineSegment2d)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attributes


color : { iteration : Int, maxIter : Int } -> String
color { iteration, maxIter } =
    Color.hsl (toFloat iteration / toFloat maxIter) 0.7 0.5
        |> Color.toCssString


line : { iteration : Int, maxIter : Int } -> LineSegment2d units coordinates -> Svg msg
line { iteration, maxIter } =
    Svg.lineSegment2d
        [ Attributes.stroke <| color { iteration = iteration, maxIter = maxIter }
        , Attributes.strokeWidth "2px"
        ]
