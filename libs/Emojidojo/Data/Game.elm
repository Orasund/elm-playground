module Emojidojo.Data.Game exposing (Game, insertResponse, json, updateCurrentPlayerResponse)

import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.String as String
import Http exposing (Error)
import Jsonstore exposing (Json)
import Task exposing (Task)


type alias Game data =
    { currentPlayer : Id
    , data : data
    }


json : Json data -> Json (Game data)
json dataJson =
    Jsonstore.object Game
        |> Jsonstore.with "currentPlayer" Id.json .currentPlayer
        |> Jsonstore.with "data" dataJson .data
        |> Jsonstore.toJson


insertResponse : Config -> { dataJson : Json data, game : Game data, roomId : Id } -> Task Error ()
insertResponse config { dataJson, game, roomId } =
    game
        |> Jsonstore.encode (json dataJson)
        |> Jsonstore.insert
            (Data.url config ++ String.openRoom ++ "/" ++ roomId ++ String.game)


updateCurrentPlayerResponse : Config -> { currentPlayer : Id, roomId : Id } -> Task Error ()
updateCurrentPlayerResponse config { currentPlayer, roomId } =
    currentPlayer
        |> Jsonstore.encode Id.json
        |> Jsonstore.insert
            (Data.url config ++ String.openRoom ++ "/" ++ roomId ++ String.game ++ String.currentPlayer)
