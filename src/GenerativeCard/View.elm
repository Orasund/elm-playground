module GenerativeCard.View exposing (line)

import Color
import Geometry.Svg as Svg
import LineSegment2d exposing (LineSegment2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


color : { iteration : Int, maxIter : Int } -> String
color { iteration } =
    let
        maxIter =
            30
    in
    Color.hsl (toFloat iteration / toFloat maxIter) 0.7 0.5
        |> Color.toCssString


line : { iteration : Int, maxIter : Int } -> LineSegment2d units coordinates -> Svg msg
line { iteration, maxIter } =
    Svg.lineSegment2d
        [ Attributes.stroke <| color { iteration = iteration, maxIter = maxIter }
        , Attributes.strokeWidth "2px"
        ]
