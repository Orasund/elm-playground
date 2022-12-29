module Data.Entity exposing (..)

import Data.Item exposing (Item)


type Entity
    = Vein Item
    | Wall
    | CrackedWall
    | Water
    | Lava
    | Actor Int
