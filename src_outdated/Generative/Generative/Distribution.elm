module Generative.Distribution exposing (Distribution, constant, gradual, normal, uniform)

import Random exposing (Generator)
import Random.Float as Random


type alias Distribution =
    Generator Float


constant : Float -> Distribution
constant =
    Random.constant


uniform : Distribution
uniform =
    Random.float 0 1


gradual : Distribution
gradual =
    let
        fractionalModBy : Float -> Float -> Float
        fractionalModBy modulus x =
            x - modulus * toFloat (floor (x / modulus))
    in
    Random.standardNormal
        |> Random.map ((+) 2 >> fractionalModBy 4 >> (+) -2 >> abs >> (\x -> x / 2))


normal : Distribution
normal =
    let
        fractionalModBy : Float -> Float -> Float
        fractionalModBy modulus x =
            x - modulus * toFloat (floor (x / modulus))
    in
    Random.standardNormal
        |> Random.map ((+) 2 >> fractionalModBy 4 >> (\x -> x / 4))
