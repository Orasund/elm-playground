module Data.Entity exposing (..)

import Data.Item exposing (Item)


type Entity
    = Vein Item
    | Wall
    | Water
    | Lava
    | Actor Int
