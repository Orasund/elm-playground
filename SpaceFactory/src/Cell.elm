module Cell exposing (..)

import Color exposing (Color)


type Building
    = Consumer Color
    | Producer Color
    | Pipe


type alias Cell =
    { building : Building, item : Maybe Int }


fromBuilding : Building -> Cell
fromBuilding building =
    { building = building, item = Nothing }
