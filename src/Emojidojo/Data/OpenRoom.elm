module Emojidojo.Data.OpenRoom exposing (OpenRoom, getListResponse, getResponse, insertResponse, json, removeResponse, updateResponse)

import Dict exposing (Dict)
import Emojidojo.Data as Data
import Emojidojo.Data.Player as Player exposing (Player)
import Emojidojo.Data.Timestamp as Timestamp exposing (Timestamp)
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias OpenRoom =
    { id : Int
    , lastUpdated : Posix
    , player : Dict String Player
    }


json : Json OpenRoom
json =
    Jsonstore.object OpenRoom
        |> Jsonstore.with "id" Jsonstore.int .id
        |> Jsonstore.with "lastUpdated"
            (Jsonstore.int |> Jsonstore.map Time.millisToPosix Time.posixToMillis)
            .lastUpdated
        |> Jsonstore.with "player" (Jsonstore.dict Player.json) .player
        |> Jsonstore.toJson


getResponse : Int -> Task Error (Maybe OpenRoom)
getResponse id =
    json
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url ++ String.openRoom ++ "/" ++ String.fromInt id)


getListResponse : Task Error (List OpenRoom)
getListResponse =
    json
        |> Jsonstore.dict
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url ++ String.openRoom)
        |> Task.map (Maybe.map Dict.values >> Maybe.withDefault [])


insertResponse : OpenRoom -> Task Error ()
insertResponse openRoom =
    openRoom
        |> Jsonstore.encode json
        |> Jsonstore.insert
            (Data.url ++ String.openRoom ++ "/" ++ String.fromInt openRoom.id)


removeResponse : Int -> Task Error ()
removeResponse id =
    Jsonstore.delete (Data.url ++ String.openRoom ++ "/" ++ String.fromInt id)


updateResponse : { roomId : Int, lastUpdated : Posix } -> Task Error ()
updateResponse { roomId, lastUpdated } =
    Timestamp.updateResponse
        (Data.url
            ++ String.openRoom
            ++ ("/" ++ String.fromInt roomId)
            ++ String.lastUpdated
        )
        lastUpdated
