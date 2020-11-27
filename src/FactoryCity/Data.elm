module FactoryCity.Data exposing (devMode, firestore, gameVersion, maxBugCycle, maxHistorySize, maxPrice, updateName, yOffset)

import Firestore exposing (Firestore)
import Firestore.Config


firestore : Firestore
firestore =
    Firestore.Config.new
        { apiKey = "AIzaSyAHNxt048Q4BFwbt_ehv4t4rxydqdc0QNc"
        , project = "elm-games"
        }
        |> Firestore.init
        |> Firestore.withCollection "factory-city"
        |> Firestore.withCollection "shop"


yOffset : Int
yOffset =
    50


maxPrice : Int
maxPrice =
    10000


maxBugCycle : Int
maxBugCycle =
    100


gameVersion : Int
gameVersion =
    1


devMode : Bool
devMode =
    True


updateName : String
updateName =
    "Evergreen"


maxHistorySize : Int
maxHistorySize =
    2 * 100
