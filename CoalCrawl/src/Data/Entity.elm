module Data.Entity exposing (..)

import AnyBag exposing (AnyBag)
import Data.Item exposing (Item)
import Data.Wagon exposing (Wagon)


type Entity
    = Vein Item
    | Wall { unstable : Bool }
    | Wagon Wagon
    | Water
    | Train
    | Rubble (List Item)
