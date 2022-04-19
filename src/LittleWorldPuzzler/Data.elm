module LittleWorldPuzzler.Data exposing (devMode, firestore, gameVersion, maxHistorySize, updateName)

import Firestore exposing (Firestore)
import Firestore.Config


gameVersion : Int
gameVersion =
    0


devMode : Bool
devMode =
    False


updateName : String
updateName =
    "Evergreen"


maxHistorySize : Int
maxHistorySize =
    2 * 100


firestore : Firestore
firestore =
    Firestore.Config.new
        { apiKey = "AIzaSyAHNxt048Q4BFwbt_ehv4t4rxydqdc0QNc"
        , project = "elm-games"
        }
        |> Firestore.init



--|> Firestore.withCollection "little-world-puzzler"
--|> Firestore.withCollection "highscore"
