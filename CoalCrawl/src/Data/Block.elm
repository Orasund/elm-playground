module Data.Block exposing (..)

import AnyBag exposing (AnyBag)
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Item exposing (Item)


type Block
    = FloorBlock Floor
    | EntityBlock Entity
