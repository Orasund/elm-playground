module FactoryCity.Data.Entry exposing (Entry, new)

import FactoryCity.Data as Data exposing (gameVersion)
import FactoryCity.Data.Game as Game exposing (EndCondition(..), Game)
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
