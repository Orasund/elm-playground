module Config exposing (..)

import StaticArray.Index as Index


defaultCellSize =
    64


smallCellSize =
    48


maxPos level =
    if level == Index.first then
        1

    else
        4
