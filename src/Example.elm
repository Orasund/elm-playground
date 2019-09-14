module Example exposing (main)

import Color
import Generative
import Generative.Distribution as Distribution exposing (Distribution)
import Generative.Shape as Shape exposing (Shape, Surface(..))
import Html exposing (Html)
import Html.Attributes as Attributes
import Random exposing (Generator)
import Svg exposing (Svg)


rect : ( Float, Float ) -> ( Float, Float ) -> ( Distribution, Distribution ) -> Generator (List (Svg msg))
rect p1 p2 dist =
    Shape.rectangle p1 p2
        |> Shape.withSurface
            (Textured
                { density = 0.15
                , distribution = dist
                , shapes =
                    \( x, y ) ->
                        let
                            size : Float
                            size =
                                sqrt ((x - 0.5) ^ 2 + (y - 0.5) ^ 2) * 3
                        in
                        Random.float 0 (2 * pi)
                            |> Random.map
                                (\angle ->
                                    Shape.rectangle ( 0, 0 ) ( 2 + size, 2 + size )
                                        |> Shape.withSurface Empty
                                        |> Shape.withColor (Color.rgba 0 0 0 0.5)
                                        |> Shape.rotateBy angle
                                )
                , border = True
                }
            )
        |> Generative.toSvg


main : Html msg
main =
    Random.step
        ([ rect ( 0, 0 ) ( 100, 100 ) ( Distribution.uniform, Distribution.uniform )
         , rect ( 110, 0 ) ( 210, 100 ) ( Distribution.uniform, Distribution.gradual )
         , rect ( 0, 110 ) ( 100, 210 ) ( Distribution.uniform, Distribution.normal )
         , rect ( 110, 110 ) ( 210, 210 ) ( Distribution.normal, Distribution.normal )
         ]
            |> Generative.toHtml [ Attributes.width 1000, Attributes.height 1000 ]
        )
        (Random.initialSeed 42)
        |> Tuple.first
