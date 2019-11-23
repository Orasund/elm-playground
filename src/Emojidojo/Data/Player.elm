module Emojidojo.Data.Player exposing (Player, insertResponse, json, removeResponse, updateResponse)

import Emojidojo.Data as Data
import Emojidojo.Data.Timestamp as Timestamp exposing (Timestamp)
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias Player =
    { id : Int
    , lastUpdated : Posix
    }


json : Json Player
json =
    Jsonstore.object Player
        |> Jsonstore.with "id" Jsonstore.int .id
        |> Jsonstore.with "lastUpdated"
            (Jsonstore.int |> Jsonstore.map Time.millisToPosix Time.posixToMillis)
            .lastUpdated
        |> Jsonstore.toJson


insertResponse : Int -> Player -> Task Error ()
insertResponse roomId player =
    player
        |> Jsonstore.encode json
        |> Jsonstore.insert
            (Data.url
                ++ String.openRoom
                ++ ("/" ++ String.fromInt roomId)
                ++ String.player
                ++ ("/" ++ String.fromInt player.id)
            )


removeResponse : { roomId : Int, playerId : Int } -> Task Error ()
removeResponse { roomId, playerId } =
    Jsonstore.delete
        (Data.url
            ++ String.openRoom
            ++ ("/" ++ String.fromInt roomId)
            ++ String.player
            ++ ("/" ++ String.fromInt playerId)
        )


updateResponse : { roomId : Int, playerId : Int, lastUpdated : Posix } -> Task Error ()
updateResponse { roomId, playerId, lastUpdated } =
    Timestamp.updateResponse
        (Data.url
            ++ String.openRoom
            ++ ("/" ++ String.fromInt roomId)
            ++ String.player
            ++ ("/" ++ String.fromInt playerId)
            ++ String.lastUpdated
        )
        lastUpdated
