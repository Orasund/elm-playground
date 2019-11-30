module Emojidojo.Data.PlayerInfo exposing (PlayerInfo, insertResponse, json, removeResponse, updateResponse)

import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.Timestamp as Timestamp
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias PlayerInfo =
    { id : Id
    , lastUpdated : Posix
    }


json : Json PlayerInfo
json =
    Jsonstore.object PlayerInfo
        |> Jsonstore.with "id" Id.json .id
        |> Jsonstore.with "lastUpdated"
            (Jsonstore.int |> Jsonstore.map Time.millisToPosix Time.posixToMillis)
            .lastUpdated
        |> Jsonstore.toJson


insertResponse : Config -> Id -> PlayerInfo -> Task Error ()
insertResponse config roomId player =
    player
        |> Jsonstore.encode json
        |> Jsonstore.insert
            (Data.url config
                ++ String.openRoom
                ++ ("/" ++ roomId)
                ++ String.player
                ++ ("/" ++ player.id)
            )


removeResponse : Config -> { roomId : Id, playerId : Id } -> Task Error ()
removeResponse config { roomId, playerId } =
    Jsonstore.delete
        (Data.url config
            ++ String.openRoom
            ++ ("/" ++ roomId)
            ++ String.player
            ++ ("/" ++ playerId)
        )


updateResponse : Config -> { roomId : Id, playerId : Id, lastUpdated : Posix } -> Task Error ()
updateResponse config { roomId, playerId, lastUpdated } =
    Timestamp.updateResponse
        (Data.url config
            ++ String.openRoom
            ++ ("/" ++ roomId)
            ++ String.player
            ++ ("/" ++ playerId)
            ++ String.lastUpdated
        )
        lastUpdated
