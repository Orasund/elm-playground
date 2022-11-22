module Data.Actor exposing (..)

import Config
import Data.Item exposing (Item)
import Data.Minecart exposing (Minecart)


type CaveType
    = WaterCave
    | CoalCave
    | IronCave
    | LavaCave


type Helper
    = Cave CaveType
    | Mine
    | Falling Item
    | Path


type Actor
    = Minecart Minecart
    | Bomb { explodesIn : Int }
    | Excavator
        { momentum : Maybe ( Int, Int )
        , hasReversed : Bool
        }
    | Helper Helper


bomb : Actor
bomb =
    Bomb { explodesIn = Config.bombExplosionTime }
