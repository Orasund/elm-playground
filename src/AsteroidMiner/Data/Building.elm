module AsteroidMiner.Data.Building exposing (BeltColor(..), Building, BuildingType(..), Command(..))

import Grid.Direction exposing (Direction)


type BuildingType
    = Mine
    | ConveyorBelt (Maybe BeltColor)
    | Container


type BeltColor
    = Red
    | Blue
    | Green
    | Yellow


type Command a
    = Idle
    | Decrease
    | Set Int
    | Send Direction
    | Transition a


type alias Building a =
    { counter : Int
    , sort : a
    }
