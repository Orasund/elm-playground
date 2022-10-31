module Data.Block exposing (..)

import Data.Item exposing (Item)


type Block
    = Ground (Maybe Item)
    | CoalVein
    | Wall
    | Train
    | Track
