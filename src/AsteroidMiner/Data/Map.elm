module AsteroidMiner.Data.Map exposing (Command, GroundType(..), Item(..), Map, Neighborhood, Square, SquareType)

import AsteroidMiner.Building exposing (BuildingType)
import AsteroidMiner.Lib.Command as Command
import AsteroidMiner.Lib.Map as Map
import AsteroidMiner.Lib.Neighborhood as Neighborhood


type GroundType
    = Empty
    | Mountain
    | OreGround


type Item
    = Stone


type alias Neighborhood =
    Neighborhood.Neighborhood (Maybe BuildingType)


type alias Command =
    Command.Command BuildingType Item


type alias SquareType =
    Map.SquareType BuildingType GroundType


type alias Square =
    Map.Square BuildingType GroundType Item


type alias Map =
    Map.Map BuildingType GroundType Item
