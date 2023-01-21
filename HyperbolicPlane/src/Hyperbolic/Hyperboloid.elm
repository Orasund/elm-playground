module Hyperbolic.Hyperboloid exposing (..)

import Internal exposing (HyperboloidPoint(..), PoincarePoint(..))


fromPoincare : PoincarePoint -> HyperboloidPoint
fromPoincare (PoincarePoint ( x, y )) =
    --https://github.com/looeee/hyperbolic-tiling/blob/main/src/utilities/mathFunctions.js
    let
        factor =
            1 / (1 - x * x - y * y)
    in
    ( 2 * factor * x
    , 2 * factor * y
    , factor * (1 + x * x + y * y)
    )
        |> HyperboloidPoint


toPoincare : HyperboloidPoint -> PoincarePoint
toPoincare (HyperboloidPoint ( x, y, z )) =
    --https://github.com/looeee/hyperbolic-tiling/blob/main/src/utilities/mathFunctions.js
    let
        factor =
            1 / (1 + z)
    in
    ( factor * x, factor * y )
        |> PoincarePoint


applyTransformation : ( ( Float, Float, Float ), ( Float, Float, Float ), ( Float, Float, Float ) ) -> HyperboloidPoint -> HyperboloidPoint
applyTransformation ( ( a, b, c ), ( d, e, f ), ( g, h, i ) ) (HyperboloidPoint ( x, y, z )) =
    ( a * x + b * y + c * z
    , d * x + e * y + f * z
    , g * x + h * y + i * z
    )
        |> HyperboloidPoint
