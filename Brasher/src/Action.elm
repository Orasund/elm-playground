module Action exposing (..)

import Tile exposing (Tile)


type Action
    = Move { from : Int, to : Int }
    | Kill Int
    | Spawn Int Tile
    | LevelCleared
    | LooseLife { killedBy : Tile }
    | Tick
    | AddPoint
    | Chain (List Action)
