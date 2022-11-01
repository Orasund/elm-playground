module Data.Entity exposing (..)

import AnyBag exposing (AnyBag)
import Data.Item exposing (Item)


type Entity
    = Vein Item
    | Wall
    | RailwayTrack
    | Wagon (AnyBag String Item)
