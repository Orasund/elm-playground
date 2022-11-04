module Data.Entity exposing (..)

import Data.Item exposing (Item)
import Data.Wagon exposing (Wagon)


type CaveType
    = WaterCave
    | RubbleCave


type Entity
    = Vein Item
    | Wall
    | Cave CaveType
    | Wagon Wagon
    | Water
    | Train
    | Rubble (List Item)


rubble : Entity
rubble =
    Rubble [ Data.Item.Coal, Data.Item.Coal, Data.Item.Iron ]
