module Emojidojo exposing (Game, config, define, withNrOfPlayers)

import Element exposing (Element)
import Emojidojo.Data.Config as Config
import Emojidojo.Main as Main
import Jsonstore exposing (Json)


type alias Game data remote msg =
    Main.Game data remote msg


type Config
    = Config Config.Config


config : { jsonstoreId : String, version : Float } -> Config
config { jsonstoreId, version } =
    Config
        { jsonstoreId = jsonstoreId
        , roomOpenInMillis = 1000 * 10
        , nrOfplayers = 2
        , version = version
        }


withNrOfPlayers : Int -> Config -> Config
withNrOfPlayers nr (Config c) =
    Config { c | nrOfplayers = nr }


define :
    { init : remote -> data
    , view : data -> Element msg
    , subscriptions : data -> Sub msg
    , update : msg -> data -> ( data, Cmd msg )
    , title : String
    , config : Config
    , remote :
        { msg : remote -> msg
        , json : Json remote
        , init : remote
        , fromModel : data -> remote
        }
    }
    -> Game data remote msg
define input =
    let
        (Config c) =
            input.config
    in
    Main.define
        { init = input.init
        , view = input.view
        , subscriptions = input.subscriptions
        , update = input.update
        , title = input.title
        , config = c
        , remote = input.remote
        }
