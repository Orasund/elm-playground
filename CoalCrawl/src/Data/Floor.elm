module Data.Floor exposing (..)

import Data.Item exposing (Item)


type Floor
    = Ground (Maybe Item)
    | Track
    | Train


ground : Floor
ground =
    Ground Nothing
