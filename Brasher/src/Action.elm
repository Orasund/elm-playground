module Action exposing (..)

import Sound exposing (Sound)
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
    | PlaySound Sound
