module Emojidojo.Data.OpenRoom exposing
    ( OpenRoom
    , getListResponse
    , getResponse
    , insertResponse
    , json
    , removeResponse
    , updateResponse
    )

import Dict exposing (Dict)
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Game as Game exposing (Game)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.PlayerInfo as PlayerInfo exposing (PlayerInfo)
import Emojidojo.Data.Timestamp as Timestamp
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias OpenRoom data =
    { id : Id
    , lastUpdated : Posix
    , player : Dict Id PlayerInfo
    , game : Maybe (Game data)
    }


json : Json data -> Json (OpenRoom data)
json dataJson =
    Jsonstore.object OpenRoom
        |> Jsonstore.with "id" Id.json .id
        |> Jsonstore.with "lastUpdated"
            (Jsonstore.int |> Jsonstore.map Time.millisToPosix Time.posixToMillis)
            .lastUpdated
        |> Jsonstore.with "player" (Jsonstore.dict PlayerInfo.json) .player
        |> Jsonstore.withMaybe "game" (Game.json dataJson) .game
        |> Jsonstore.toJson


getResponse : Config -> { roomId : Id, dataJson : Json data } -> Task Error (Maybe (OpenRoom data))
getResponse config { roomId, dataJson } =
    json dataJson
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url config ++ String.openRoom ++ "/" ++ roomId)


getListResponse : Config -> Json data -> Task Error (List (OpenRoom data))
getListResponse config dataJson =
    json dataJson
        |> Jsonstore.dict
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url config ++ String.openRoom)
        |> Task.map (Maybe.map Dict.values >> Maybe.withDefault [])


insertResponse : Config -> Json data -> OpenRoom data -> Task Error ()
insertResponse config dataJson openRoom =
    openRoom
        |> Jsonstore.encode (json dataJson)
        |> Jsonstore.insert
            (Data.url config ++ String.openRoom ++ "/" ++ openRoom.id)


removeResponse : Config -> Id -> Task Error ()
removeResponse config id =
    Jsonstore.delete (Data.url config ++ String.openRoom ++ "/" ++ id)


updateResponse : Config -> { roomId : Id, lastUpdated : Posix } -> Task Error ()
updateResponse config { roomId, lastUpdated } =
    Timestamp.updateResponse
        (Data.url config
            ++ String.openRoom
            ++ ("/" ++ roomId)
            ++ String.lastUpdated
        )
        lastUpdated
