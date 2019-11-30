module Emojidojo.Data.Timestamp exposing (Timestamp, json, updateResponse)

import Emojidojo.Data as Data
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias Timestamp =
    Posix


json : Json Timestamp
json =
    Jsonstore.int
        |> Jsonstore.map Time.millisToPosix Time.posixToMillis


updateResponse : String -> Timestamp -> Task Error ()
updateResponse url timestamp =
    Jsonstore.update
        { url = url
        , decoder = json |> Jsonstore.decode
        , value = Maybe.map (always (timestamp |> Jsonstore.encode json))
        }
