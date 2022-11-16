module Data.Zoom exposing (..)


type Zoom
    = Zoom Float


none : Zoom
none =
    Zoom 1


get : Zoom -> Float
get (Zoom v) =
    v


fromPercent : Int -> Zoom
fromPercent n =
    let
        power =
            toFloat n / 100
    in
    Zoom (3 ^ power)
