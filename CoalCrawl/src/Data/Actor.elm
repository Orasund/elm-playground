module Data.Actor exposing (..)

import Data.Bomb exposing (Bomb)
import Data.Entity exposing (Entity)
import Data.Excavator exposing (Excavator)
import Data.Minecart exposing (Minecart)
import Data.Train exposing (Train)


type CaveType
    = WaterCave
    | CoalCave
    | IronCave
    | LavaCave


type Helper
    = Cave CaveType
    | Mine
    | Falling Entity
    | Path


type Actor
    = Minecart Minecart
    | Bomb Bomb
    | Excavator Excavator
    | Helper Helper
    | Train Train
