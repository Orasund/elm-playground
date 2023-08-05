module Config exposing (..)

import StaticArray.Index as Index


defaultCellSize =
    48


smallCellSize =
    32


maxPos level =
    if level == Index.first then
        1

    else
        4
