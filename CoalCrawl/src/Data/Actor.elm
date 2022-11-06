module Data.Actor exposing (..)

import Data.Wagon exposing (Wagon)


type CaveType
    = WaterCave
    | RubbleCave
    | CoalCave


type Actor
    = Wagon Wagon
    | Cave CaveType
