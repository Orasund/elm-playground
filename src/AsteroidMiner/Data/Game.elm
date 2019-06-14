module AsteroidMiner.Data.Game exposing (Game, GroundType(..), Item, Map, Square, SquareType)

import AsteroidMiner.Data.Building exposing (BuildingType)
import AsteroidMiner.Data.Comet exposing (Comet)
import AsteroidMiner.Data.Map as Map


type GroundType
    = Empty
    | Mountain
    | OreGround


type alias Item =
    Never


type alias SquareType =
    Map.SquareType BuildingType GroundType


type alias Square =
    Map.Square BuildingType GroundType Item


type alias Map =
    Map.Map BuildingType GroundType Item


type alias Game =
    { comet : Comet
    , map : Map
    }
