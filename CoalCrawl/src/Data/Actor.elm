module Data.Actor exposing (..)

import Data.Bomb exposing (Bomb)
import Data.Excavator exposing (Excavator)
import Data.Item exposing (Item)
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
    | Falling Item
    | Path


type Actor
    = Minecart Minecart
    | Bomb Bomb
    | Excavator Excavator
    | Helper Helper
    | Train Train
    | WaterSource
