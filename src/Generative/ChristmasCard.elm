module Generative.ChristmasCard exposing (main)

import Color
import Generative
import Generative.Distribution as Distribution
import Generative.Point as Point exposing (Point)
import Generative.Shape as Shape exposing (Shape, Surface(..))
import Html exposing (Html)
import Html.Attributes as Attributes
import Random exposing (Generator)
import Svg exposing (Svg)


width : Int
width =
    500


height : Int
height =
    round <| 1.2 * toFloat width


wood : Generator Shape
wood =
    Random.map2
        (\angle size ->
            Shape.rectangle ( -2, 0 ) ( 2, size )
                |> Shape.withSurface Filled
                |> Shape.withColor (Color.hsla 0.3 0.2 1 1)
                {- (Color.hsla
                       0.03
                       0.5
                       0.2
                       0.5
                   )
                -}
                |> Shape.rotateBy angle
        )
        (Random.float -(pi / 4) (pi / 4))
        (Random.float 10 20)


leaf : Generator Shape
leaf =
    Random.map4
        (\isLeaf angle size color ->
            if isLeaf < 2 then
                Shape.circle 10 ( 0, 0 )
                    |> Shape.withColor (Color.hsla 0.3 0.2 1 1)
                    {- (Color.hsla
                           (color
                               |> (*) 10
                               |> round
                               |> (+) -6
                               |> modBy 10
                               |> toFloat
                               |> (\x -> x / 10)
                           )
                           0.7
                           0.5
                           1
                       )
                    -}
                    |> Shape.withSurface Filled

            else
                Shape.regular 3 size ( 0, 0 )
                    --|> Shape.withColor (Color.hsla 0.3 0.2 0.4 color)
                    |> Shape.withColor (Color.hsla 0.3 0.2 1 1)
                    |> Shape.rotateBy angle
                    |> Shape.withSurface Filled
        )
        (Random.int 0 80)
        (Random.float (pi - pi / 16) (pi + pi / 16))
        (Random.float 5 10)
        (Random.float 0.5 0.7)


stemPart : Generator Shape
stemPart =
    Random.float (pi - pi / 4) (pi + pi / 4)
        |> Random.map
            (\angle ->
                Shape.rectangle
                    ( -(toFloat width / 32), -(toFloat height / 32) )
                    ( toFloat width / 32, toFloat height / 32 )
                    |> Shape.rotateBy angle
                    |> Shape.withSurface
                        (Textured
                            { density = 0.05
                            , distribution = ( Distribution.uniform, Distribution.uniform )
                            , shapes =
                                always
                                    wood
                            , border = False
                            }
                        )
            )


stem : Generator (List (Svg msg))
stem =
    Shape.rectangle
        ( (toFloat width / 2) - (toFloat width / 16), toFloat (height * 2) / 6 )
        ( (toFloat width / 2) + (toFloat width / 16), toFloat (height * 4) / 6 )
        |> Shape.withSurface
            (Textured
                { density = 0.005
                , distribution = ( Distribution.uniform, Distribution.uniform )
                , shapes =
                    always
                        stemPart
                , border = False
                }
            )
        |> Generative.toSvg


treePart : Generator Shape
treePart =
    Random.float (pi - pi / 4) (pi + pi / 4)
        |> Random.map
            (\angle ->
                Shape.regular 3 (toFloat height / 16) ( 0, 0 )
                    |> Shape.rotateBy angle
                    |> Shape.withSurface
                        (Textured
                            { density = 0.015
                            , distribution = ( Distribution.uniform, Distribution.uniform )
                            , shapes =
                                always
                                    leaf
                            , border = False
                            }
                        )
            )


tree : Generator (List (Svg msg))
tree =
    Shape.regular 3 (toFloat height / 3) ( toFloat width / 2, toFloat height / 6 )
        |> Shape.rotateBy pi
        |> Shape.withSurface
            (Textured
                { density = 0.002
                , distribution = ( Distribution.uniform, Distribution.uniform )
                , shapes =
                    always
                        treePart
                , border = False
                }
            )
        |> Generative.toSvg


f : List Point
f =
    [ ( 7, 2 )
    , ( 0, 3 )
    , ( 2, 1 )
    , ( 1, 9 )
    , ( 2, 9 )
    , ( 0, 5 )
    , ( 7, 6 )
    ]


main : Html msg
main =
    Random.step
        ([ tree
         , stem
         ]
            |> List.append
                ([]
                    --f
                    |> Point.smoothen 5
                    |> (\list ->
                            list
                                |> List.foldl
                                    (\_ ( l, out ) ->
                                        case l of
                                            p1 :: p2 :: tail ->
                                                ( p2 :: tail
                                                , (Shape.fromPoints [ p1, p2 ] |> Generative.toSvg)
                                                    :: out
                                                )

                                            _ ->
                                                ( [], out )
                                    )
                                    ( list
                                        |> List.map
                                            (Tuple.mapBoth
                                                ((*) (toFloat <| height // 16))
                                                ((*) (toFloat <| height // 16))
                                            )
                                    , []
                                    )
                                |> Tuple.second
                       )
                )
            |> Generative.toHtml [ Attributes.width width, Attributes.height height ]
        )
        (Random.initialSeed 66)
        |> Tuple.first
