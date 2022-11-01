module Data.Entity exposing (..)

import AnyBag exposing (AnyBag)
import Data.Item exposing (Item)


type Entity
    = Vein Item
    | Wall { unstable : Bool }
    | RailwayTrack
    | Wagon (AnyBag String Item)
    | Water
