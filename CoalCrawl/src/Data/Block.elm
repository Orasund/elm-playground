module Data.Block exposing (..)

import Data.Actor exposing (Actor)
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)


type Block
    = FloorBlock Floor
    | EntityBlock Entity
    | ActorBlock ( Int, Actor )
