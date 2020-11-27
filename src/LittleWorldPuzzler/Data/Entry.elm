module LittleWorldPuzzler.Data.Entry exposing (Entry, codec, new)

import Firestore.Codec as Codec exposing (Codec)
import LittleWorldPuzzler.Data as Data
import LittleWorldPuzzler.Data.Game exposing (Game)
import UndoList exposing (UndoList)


type alias Entry =
    { version : Int
    , score : Int
    }


new : UndoList Game -> Entry
new history =
    { version = Data.gameVersion
    , score = history.present.score
    }



{------------------------
   Json
------------------------}
{--jsonUndoList : Json (UndoList Game)
jsonUndoList =
    Jsonstore.object UndoList
        |> Jsonstore.withList "past" Game.json (.past >> List.take Data.maxHistorySize)
        |> Jsonstore.with "present" Game.json .present
        |> Jsonstore.withList "future" Game.json .future
        |> Jsonstore.toJson
--}
{--json : Json Entry
json =
    Jsonstore.object Entry
        |> Jsonstore.with "history" jsonUndoList .history
        |> Jsonstore.with "version" Jsonstore.int .version
        |> Jsonstore.with "score" Jsonstore.int .score
        |> Jsonstore.toJson
--}


codec : Codec Entry
codec =
    Codec.document Entry
        |> Codec.required "version" .version Codec.int
        |> Codec.required "score" .score Codec.int
        |> Codec.build
