module Example exposing (main)

import Color
import Generative
import Generative.Distribution as Distribution exposing (Distribution)
import Generative.Shape as Shape exposing (Shape, Surface(..))
import Html exposing (Html)
import Html.Attributes as Attributes
import Random exposing (Generator)
import Svg exposing (Svg)


square : ( Float, Float ) -> ( Distribution, Distribution ) -> Generator (List (Svg msg))
square p1 dist =
    --Shape.regular 5 100 p1
    Shape.circle 100 p1
        |> Shape.rotateBy (pi / 3)
        |> Shape.withColor
            (Color.hsla
                0.05
                0.3
                0.5
                0.5
            )
        |> Shape.withSurface
            (Textured
                { density = 0.05
                , distribution = dist
                , shapes =
                    \( x, y ) ->
                        let
                            distance : Float
                            distance =
                                sqrt ((x - 0.5) ^ 2 + (y - 0.5) ^ 2)

                            n : Int
                            n =
                                (+) 2 <| round <| x * 4
                        in
                        Random.map2
                            (\angle size ->
                                Shape.circle size ( 0, 0 )
                                    |> Shape.withSurface Filled
                                    |> Shape.withColor
                                        (Color.hsla
                                            (0.45 + y * x * 0.1)
                                            0.3
                                            (0.1 + 0.7 * (1 - distance))
                                            0.5
                                        )
                                    |> Shape.rotateBy angle
                            )
                            (Random.float 0 (2 * pi))
                            (Random.float 1 5)
                , border = False
                }
            )
        |> Generative.toSvg


main : Html msg
main =
    Random.step
        ([ square ( 100, 100 ) ( Distribution.uniform, Distribution.uniform )
         , square ( 300, 100 ) ( Distribution.uniform, Distribution.gradual )
         , square ( 100, 300 ) ( Distribution.uniform, Distribution.normal )
         , square ( 300, 300 ) ( Distribution.normal, Distribution.normal )
         ]
            |> Generative.toHtml [ Attributes.width 1000, Attributes.height 1000 ]
        )
        (Random.initialSeed 42)
        |> Tuple.first
