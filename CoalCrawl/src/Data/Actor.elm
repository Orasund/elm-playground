module Data.Actor exposing (..)

import Config
import Data.Wagon exposing (Wagon)


type CaveType
    = WaterCave
    | RubbleCave
    | CoalCave
    | IronCave


type Actor
    = Wagon Wagon
    | Cave CaveType
    | Bomb { explodesIn : Int }


bomb : Actor
bomb =
    Bomb { explodesIn = Config.bombExplosionTime }
