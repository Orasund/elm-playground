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
    | Excavator
        { momentum : Maybe ( Int, Int )
        , hasReversed : Bool
        }


bomb : Actor
bomb =
    Bomb { explodesIn = Config.bombExplosionTime }
