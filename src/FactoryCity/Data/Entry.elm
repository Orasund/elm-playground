module FactoryCity.Data.Entry exposing (Entry, new)

import FactoryCity.Data exposing (gameVersion)
import FactoryCity.Data.Game exposing (Game)
import UndoList exposing (UndoList)


type alias Entry =
    { history : UndoList Game
    , version : Int
    , score : Int
    }


new : UndoList Game -> Entry
new history =
    { history = history
    , version = gameVersion
    , score = history.present.score
    }
