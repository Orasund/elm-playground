module Data.Entity exposing (..)

import Data.Item exposing (Item)
import Data.Wagon exposing (Wagon)


type CaveType
    = WaterCave


type Entity
    = Vein Item
    | Wall
    | Cave CaveType
    | Wagon Wagon
    | Water
    | Train
    | Rubble (List Item)
