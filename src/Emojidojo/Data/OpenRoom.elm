module Emojidojo.Data.OpenRoom exposing
    ( OpenRoom
    , getListResponse
    , getResponse
    , insertGameIdResponse
    , insertResponse
    , json
    , removeResponse
    , updateResponse
    )

import Dict exposing (Dict)
import Emojidojo.Data as Data
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.PlayerInfo as PlayerInfo exposing (PlayerInfo)
import Emojidojo.Data.Timestamp as Timestamp
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias OpenRoom =
    { id : Id
    , lastUpdated : Posix
    , player : Dict Id PlayerInfo
    , gameId : Maybe Id
    }


json : Json OpenRoom
json =
    Jsonstore.object OpenRoom
        |> Jsonstore.with "id" Id.json .id
        |> Jsonstore.with "lastUpdated"
            (Jsonstore.int |> Jsonstore.map Time.millisToPosix Time.posixToMillis)
            .lastUpdated
        |> Jsonstore.with "player" (Jsonstore.dict PlayerInfo.json) .player
        |> Jsonstore.withMaybe "gameId" Id.json .gameId
        |> Jsonstore.toJson


insertGameIdResponse : { gameId : Id, roomId : Id } -> Task Error ()
insertGameIdResponse { gameId, roomId } =
    gameId
        |> Jsonstore.encode Id.json
        |> Jsonstore.insert
            (Data.url ++ String.openRoom ++ "/" ++ roomId ++ String.gameId)


getResponse : Id -> Task Error (Maybe OpenRoom)
getResponse id =
    json
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url ++ String.openRoom ++ "/" ++ id)


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
            (Data.url ++ String.openRoom ++ "/" ++ openRoom.id)


removeResponse : Id -> Task Error ()
removeResponse id =
    Jsonstore.delete (Data.url ++ String.openRoom ++ "/" ++ id)


updateResponse : { roomId : Id, lastUpdated : Posix } -> Task Error ()
updateResponse { roomId, lastUpdated } =
    Timestamp.updateResponse
        (Data.url
            ++ String.openRoom
            ++ ("/" ++ roomId)
            ++ String.lastUpdated
        )
        lastUpdated
