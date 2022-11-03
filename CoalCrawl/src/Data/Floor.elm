module Data.Floor exposing (..)

import Data.Item exposing (Item)


type Floor
    = Ground (Maybe Item)
    | RailwayTrack
    | Track


ground : Floor
ground =
    Ground Nothing
