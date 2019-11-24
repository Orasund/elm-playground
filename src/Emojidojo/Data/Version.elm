module Emojidojo.Data.Version exposing (Version, getResponse, insertResponse, json)

import Emojidojo.Data as Data
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)


type alias Version =
    Float


json : Json Version
json =
    Jsonstore.float


getResponse : Task Error (Maybe Version)
getResponse =
    json
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url ++ String.version)


insertResponse : Task Error ()
insertResponse =
    Data.version
        |> Jsonstore.encode json
        |> Jsonstore.insert (Data.url ++ String.version)
