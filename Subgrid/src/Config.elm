module Config exposing (..)

import StaticArray.Index as Index


bigCellSize =
    64


midCellSize =
    48


smallCellSize =
    32


maxPos level =
    if level == Index.first then
        1

    else
        4
