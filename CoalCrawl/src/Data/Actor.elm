module Data.Actor exposing (..)

import Config
import Data.Item exposing (Item)
import Data.Minecart exposing (Minecart)


type CaveType
    = WaterCave
    | CoalCave
    | IronCave
    | LavaCave


type Actor
    = Minecart Minecart
    | Cave CaveType
    | Mine
    | Bomb { explodesIn : Int }
    | Falling Item
    | Path


bomb : Actor
bomb =
    Bomb { explodesIn = Config.bombExplosionTime }
