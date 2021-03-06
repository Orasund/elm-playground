module Emojidojo.Data.Game exposing (Game, getListResponse, getResponse, insertResponse, json, map, removeResponse, updateCurrentPlayerResponse, updateLastUpdatedResponse, updateResponse)

import Dict exposing (Dict)
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.Player as Player exposing (Player)
import Emojidojo.Data.Timestamp as Timestamp
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)
import Time exposing (Posix)


type alias Game data =
    { id : Id
    , lastUpdated : Posix
    , player : Dict Id Player
    , currentPlayer : Id
    , data : data
    }


map : data -> Game remote -> Game data
map data game =
    { id = game.id
    , lastUpdated = game.lastUpdated
    , player = game.player
    , currentPlayer = game.currentPlayer
    , data = data
    }


json : Json data -> Json (Game data)
json dataJson =
    Jsonstore.object Game
        |> Jsonstore.with "id" Id.json .id
        |> Jsonstore.with "lastUpdated"
            (Jsonstore.int |> Jsonstore.map Time.millisToPosix Time.posixToMillis)
            .lastUpdated
        |> Jsonstore.with "player" (Jsonstore.dict Player.json) .player
        |> Jsonstore.with "currentPlayer" Id.json .currentPlayer
        |> Jsonstore.with "data" dataJson .data
        |> Jsonstore.toJson


getResponse : Config -> { gameId : Id, jsonData : Json data } -> Task Error (Maybe (Game data))
getResponse config { gameId, jsonData } =
    json jsonData
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url config ++ String.game ++ "/" ++ gameId)


getListResponse : Config -> Json data -> Task Error (List (Game data))
getListResponse config jsonData =
    json jsonData
        |> Jsonstore.dict
        |> Jsonstore.decode
        |> Jsonstore.get (Data.url config ++ String.game)
        |> Task.map (Maybe.map Dict.values >> Maybe.withDefault [])


insertResponse : Config -> { dataJson : Json data, game : Game data, gameId : Id } -> Task Error ()
insertResponse config { dataJson, game, gameId } =
    game
        |> Jsonstore.encode (json dataJson)
        |> Jsonstore.insert
            (Data.url config ++ String.game ++ "/" ++ gameId)


removeResponse : Config -> Id -> Task Error ()
removeResponse config id =
    Jsonstore.delete (Data.url config ++ String.game ++ "/" ++ id)


updateResponse : Config -> { gameId : Id, jsonData : Json data, remote : data } -> Task Error ()
updateResponse config { gameId, jsonData, remote } =
    Jsonstore.update
        { url = Data.url config ++ String.game ++ "/" ++ gameId ++ String.data
        , decoder = jsonData |> Jsonstore.decode
        , value = Maybe.map (always (remote |> Jsonstore.encode jsonData))
        }


updateCurrentPlayerResponse : Config -> { currentPlayer : Id, gameId : Id } -> Task Error ()
updateCurrentPlayerResponse config { currentPlayer, gameId } =
    Jsonstore.update
        { url = Data.url config ++ String.game ++ "/" ++ gameId ++ String.currentPlayer
        , decoder = Id.json |> Jsonstore.decode
        , value = Maybe.map (always (currentPlayer |> Jsonstore.encode Id.json))
        }


updateLastUpdatedResponse : Config -> { gameId : Id, lastUpdated : Posix } -> Task Error ()
updateLastUpdatedResponse config { gameId, lastUpdated } =
    Timestamp.updateResponse
        (Data.url config
            ++ String.game
            ++ ("/" ++ gameId)
            ++ String.lastUpdated
        )
        lastUpdated
