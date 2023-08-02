module Config exposing (..)

import StaticArray.Index as Index


cellSize =
    64


maxPos level =
    if level == Index.first then
        1

    else
        4
