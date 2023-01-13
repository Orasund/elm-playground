module Example exposing (..)

{-| Test example taken from the paper

    Hyperbolic Trigonometry and its Application in the Poincaré Ball Model of Hyperbolic Geometry

-}

import Expect exposing (Expectation)
import Hyperbolic exposing (PoincareVector(..))
import Test exposing (..)


a : PoincareVector
a =
    Hyperbolic.unsafePoincareVectorFromRecord { x = -0.67, y = 0.2 }


b : PoincareVector
b =
    Hyperbolic.unsafePoincareVectorFromRecord { x = -0.16, y = -0.3 }


c : PoincareVector
c =
    Hyperbolic.unsafePoincareVectorFromRecord { x = -0.3, y = 0.57950149830724 }


equal : PoincareVector -> PoincareVector -> Expectation
equal vec1 vec2 =
    let
        eps =
            0.001
    in
    ( Hyperbolic.unsafePoincareVectorToRecord vec1
    , Hyperbolic.unsafePoincareVectorToRecord vec2
    )
        |> Expect.all
            [ \( v1, v2 ) -> Expect.within (Expect.Absolute eps) v1.x v2.x
            , \( v1, v2 ) -> Expect.within (Expect.Absolute eps) v1.y v2.y
            ]


suite : Test
suite =
    let
        vecA =
            c
                |> Hyperbolic.vectorTo b
    in
    [ test "vector A is correct"
        (\() ->
            vecA
                |> equal
                    (Hyperbolic.unsafePoincareVectorFromRecord
                        { x = 0.00237035916884
                        , y = 0.78080386796326
                        }
                    )
        )
    ]
        |> describe "construct vector"
