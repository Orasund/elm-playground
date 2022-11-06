module Data.Entity exposing (..)

import Data.Item exposing (Item)


type Entity
    = Vein Item
    | Wall
    | Water
    | Train
    | Rubble (List Item)
    | Actor Int


rubble : Entity
rubble =
    Rubble [ Data.Item.Coal, Data.Item.Iron ]
