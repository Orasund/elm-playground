module Ruz.Config exposing (..)

import Color exposing (Color)


boardSize : Float
boardSize =
    350


red : Color
red =
    Color.rgba 1 0 0 0.125


green : Color
green =
    Color.rgba 0 1 0 0.7


yellow : Color
yellow =
    Color.rgba 1 1 0 0.5


size : Int
size =
    4


startingPos : ( Int, Int )
startingPos =
    ( size // 2, size - 1 )


playerId : Int
playerId =
    -1
