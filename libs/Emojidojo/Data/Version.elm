module Emojidojo.Data.Version exposing (Version, getResponse, insertResponse, json)

import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)


type alias Version =
    Float


json : Json Version
json =
    Jsonstore.float


getResponse : Config -> Task Error (Maybe Version)
getResponse config =
    json
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url config ++ String.version)


insertResponse : Config -> Task Error ()
insertResponse config =
    config.version
        |> Jsonstore.encode json
        |> Jsonstore.insert (Data.url config ++ String.version)
