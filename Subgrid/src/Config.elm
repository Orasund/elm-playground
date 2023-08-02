module Config exposing (..)

import Level exposing (Level(..))


cellSize =
    64


maxPos level =
    case level of
        Level1 ->
            1

        Level2 ->
            4

        Level3 ->
            4

        Level4 ->
            5
