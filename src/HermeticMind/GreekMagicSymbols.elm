module HermeticMind.GreekMagicSymbols exposing (main)

import Angle
import Arc2d
import Direction2d
import Geometry.Svg as Svg
import HermeticMind.Data.Alphabet as Alphabet
import HermeticMind.Data.Turtle as Turtle exposing (Turtle)
import HermeticMind.View.GreekMagicSymbol as GreekMagicSymbol
import Html exposing (Html)
import LineSegment2d
import Point2d
import Quantity exposing (Quantity(..))
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d


main : Html msg
main =
    let
        size =
            9
    in
    Alphabet.asList
        |> List.filterMap (GreekMagicSymbol.fromChar size)
        |> Html.div []
