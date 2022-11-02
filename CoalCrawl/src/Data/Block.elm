module Data.Block exposing (..)

import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)


type Block
    = FloorBlock Floor
    | EntityBlock Entity
