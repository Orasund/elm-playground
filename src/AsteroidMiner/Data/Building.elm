module AsteroidMiner.Data.Building exposing (BeltColor(..), Building, BuildingType(..), Command(..), isInput, isOutput, isWorkingConveyorBelt, toolToBuilding)

import AsteroidMiner.View.GUI as GUI exposing (Tool)
import Grid.Direction exposing (Direction)


type BuildingType
    = Mine
    | ConveyorBelt (Maybe ( BeltColor, Direction ))
    | Container


isOutput : BuildingType -> Bool
isOutput sort =
    case sort of
        Mine ->
            True

        _ ->
            False


isInput : BuildingType -> Bool
isInput sort =
    case sort of
        Container ->
            True

        _ ->
            False


toolToBuilding : Tool -> Maybe BuildingType
toolToBuilding selected =
    case selected of
        GUI.Delete ->
            Nothing

        GUI.Mine ->
            Just Mine

        GUI.ConveyorBelt ->
            Just <| ConveyorBelt Nothing

        GUI.Container ->
            Just <| Container


isWorkingConveyorBelt : BeltColor -> BuildingType -> Bool
isWorkingConveyorBelt color sort =
    case sort of
        ConveyorBelt (Just ( c, _ )) ->
            c == color

        _ ->
            False


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
