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
import Emojidojo.Data.Config exposing (Config)
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


insertGameIdResponse : Config -> { gameId : Id, roomId : Id } -> Task Error ()
insertGameIdResponse config { gameId, roomId } =
    gameId
        |> Jsonstore.encode Id.json
        |> Jsonstore.insert
            (Data.url config ++ String.openRoom ++ "/" ++ roomId ++ String.gameId)


getResponse : Config -> Id -> Task Error (Maybe OpenRoom)
getResponse config id =
    json
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url config ++ String.openRoom ++ "/" ++ id)


getListResponse : Config -> Task Error (List OpenRoom)
getListResponse config =
    json
        |> Jsonstore.dict
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url config ++ String.openRoom)
        |> Task.map (Maybe.map Dict.values >> Maybe.withDefault [])


insertResponse : Config -> OpenRoom -> Task Error ()
insertResponse config openRoom =
    openRoom
        |> Jsonstore.encode json
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
