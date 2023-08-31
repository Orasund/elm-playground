module Cell exposing (..)

import Config
import Crop exposing (Crop)


type alias Cell =
    { age : Int
    , crop : Maybe Crop
    , soilHealth : Int
    }


soil : Cell
soil =
    { age = 0
    , soilHealth = Config.maxSoilHealth
    , crop = Nothing
    }


soilWithHealth : Int -> Cell
soilWithHealth soilHealth =
    { soil | soilHealth = soilHealth }


addCrop : Crop -> Cell -> Cell
addCrop crop cell =
    { cell
        | age = 0
        , crop = Just crop
    }
