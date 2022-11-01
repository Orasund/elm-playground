module Data.Block exposing (..)

import Data.Item exposing (Item)


type Block
    = Ground (Maybe Item)
    | Vein Item
    | Wall
    | Train
    | Track
    | Wagon (List Item)
