module Data.Actor exposing (..)

import Config
import Data.Wagon exposing (Wagon)


type CaveType
    = WaterCave
    | CoalCave
    | IronCave
    | GoldCave


type Actor
    = Wagon Wagon
    | Cave CaveType
    | Mine
    | Bomb { explodesIn : Int }
    | FallingCoal


bomb : Actor
bomb =
    Bomb { explodesIn = Config.bombExplosionTime }
