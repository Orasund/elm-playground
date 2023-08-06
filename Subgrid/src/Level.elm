module Level exposing (..)

import StaticArray.Index as Index exposing (Five, Index)
import StaticArray.Length as Length exposing (Length)


type alias LevelAmount =
    Five


type alias Level =
    Index LevelAmount


maxLevel : Length LevelAmount
maxLevel =
    Length.five


previous : Level -> Maybe Level
previous level =
    level |> Index.decrease


next : Level -> Maybe Level
next level =
    level |> Index.increase maxLevel


toString : Level -> String
toString level =
    level |> Index.toInt |> (+) 1 |> String.fromInt
