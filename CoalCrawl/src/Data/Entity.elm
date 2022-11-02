module Data.Entity exposing (..)

import Data.Item exposing (Item)
import Data.Wagon exposing (Wagon)


type Entity
    = Vein Item
    | Wall { unstable : Bool }
    | RailwayTrack
    | Wagon Wagon
    | Water
