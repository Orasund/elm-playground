module Emojidojo.Data.Player exposing (Player, insertResponse, json, removeResponse, updateResponse)

import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.Timestamp as Timestamp
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias Player =
    { id : Id
    , lastUpdated : Posix
    }


json : Json Player
json =
    Jsonstore.object Player
        |> Jsonstore.with "id" Id.json .id
        |> Jsonstore.with "lastUpdated"
            (Jsonstore.int |> Jsonstore.map Time.millisToPosix Time.posixToMillis)
            .lastUpdated
        |> Jsonstore.toJson


insertResponse : Config -> Id -> Player -> Task Error ()
insertResponse config gameId player =
    player
        |> Jsonstore.encode json
        |> Jsonstore.insert
            (Data.url config
                ++ String.game
                ++ ("/" ++ gameId)
                ++ String.player
                ++ ("/" ++ player.id)
            )


removeResponse : Config -> { gameId : Id, playerId : Id } -> Task Error ()
removeResponse config { gameId, playerId } =
    Jsonstore.delete
        (Data.url config
            ++ String.game
            ++ ("/" ++ gameId)
            ++ String.player
            ++ ("/" ++ playerId)
        )


updateResponse : Config -> { gameId : Id, playerId : Id, lastUpdated : Posix } -> Task Error ()
updateResponse config { gameId, playerId, lastUpdated } =
    Timestamp.updateResponse
        (Data.url config
            ++ String.game
            ++ ("/" ++ gameId)
            ++ String.player
            ++ ("/" ++ playerId)
            ++ String.lastUpdated
        )
        lastUpdated
