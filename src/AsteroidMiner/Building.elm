module AsteroidMiner.Building exposing (BeltColor(..), Building, BuildingType(..), Code(..), isColoredConveyorBelt, isConveyorBelt, isConveyorBeltColored, isInput, isOutput, toolToBuilding)

import AsteroidMiner.View.GUI as GUI exposing (Tool)
import Grid.Direction exposing (Direction)


type alias Building a =
    { value : Int
    , sort : a
    }


type BeltColor
    = Red
    | Blue
    | Green
    | Yellow


type Code
    = Invalid
    | InputFound
    | Try BeltColor
    | Failed BeltColor


type BuildingType
    = Mine
    | ConveyorBelt Code
    | ColoredConveyorBelt BeltColor Direction
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

        GUI.PickUp ->
            Nothing

        GUI.Mine ->
            Just Mine

        GUI.ConveyorBelt ->
            Just <| ConveyorBelt Invalid

        GUI.Container ->
            Just <| Container


isConveyorBelt : BuildingType -> Bool
isConveyorBelt sort =
    case sort of
        ConveyorBelt _ ->
            True

        _ ->
            False


isColoredConveyorBelt : BuildingType -> Bool
isColoredConveyorBelt sort =
    case sort of
        ColoredConveyorBelt _ _ ->
            True

        _ ->
            False


isConveyorBeltColored : BeltColor -> BuildingType -> Bool
isConveyorBeltColored color sort =
    case sort of
        ColoredConveyorBelt c _ ->
            c == color

        _ ->
            False
