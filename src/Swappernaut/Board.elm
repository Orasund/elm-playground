module Swappernaut.Board exposing (level)

import Grid exposing (Grid)
import Random exposing (Generator)
import Random.List as Random
import Swappernaut.Square exposing (Square(..))


level : Int -> List (List (Maybe Square))
level lv =
    let
        o : Maybe Square
        o =
            Nothing

        g : Maybe Square
        g =
            Just Goal

        w : Maybe Square
        w =
            Just (Wall False)
    in
    [ [ g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    ]
