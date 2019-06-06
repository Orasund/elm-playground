module LittleWorldPuzzler.Data.Entry exposing (Entry, json, new)

import Jsonstore exposing (Json)
import LittleWorldPuzzler.Data as Data exposing (gameVersion)
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
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



{------------------------
   Json
------------------------}


jsonUndoList : Json (UndoList Game)
jsonUndoList =
    Jsonstore.object UndoList
        |> Jsonstore.withList "past" Game.json (.past >> List.take Data.maxHistorySize)
        |> Jsonstore.with "present" Game.json .present
        |> Jsonstore.withList "future" Game.json .future
        |> Jsonstore.toJson


json : Json Entry
json =
    Jsonstore.object Entry
        |> Jsonstore.with "history" jsonUndoList .history
        |> Jsonstore.with "version" Jsonstore.int .version
        |> Jsonstore.with "score" Jsonstore.int .score
        |> Jsonstore.toJson
