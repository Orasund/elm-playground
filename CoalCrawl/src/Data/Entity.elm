module Data.Entity exposing (..)

import Data.Item exposing (Item)


type CaveType
    = WaterCave
    | RubbleCave


type Entity
    = Vein Item
    | Wall
    | Cave CaveType
    | Water
    | Train
    | Rubble (List Item)
    | Actor Int


rubble : Entity
rubble =
    Rubble [ Data.Item.Coal, Data.Item.Coal, Data.Item.Iron ]
