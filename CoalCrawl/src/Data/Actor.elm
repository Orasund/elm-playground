module Data.Actor exposing (..)

import Data.Bomb exposing (Bomb)
import Data.Entity exposing (Entity)
import Data.Minecart exposing (Minecart)
import Data.Momentum exposing (Momentum)
import Data.Train exposing (Train)


type CaveType
    = WaterCave
    | CoalCave
    | IronCave
    | LavaCave
    | CollapsedCave


type Helper
    = Cave CaveType
    | Mine
    | Falling Entity
    | Path


type Actor
    = Minecart Minecart
    | Bomb Bomb
    | Helper Helper
    | Train Train
    | MovingWater Momentum
