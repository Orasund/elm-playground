module Emojidojo exposing (Game, config, define, withNrOfPlayers)

import Element exposing (Element)
import Emojidojo.Data.Config as Config
import Emojidojo.Main as Main


type alias Game data msg =
    Main.Game data msg


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


define : { init : data, view : data -> Element msg, title : String, config : Config } -> Game data msg
define input =
    let
        (Config c) =
            input.config
    in
    Main.define
        { init = input.init
        , view = input.view
        , title = input.title
        , config = c
        }
