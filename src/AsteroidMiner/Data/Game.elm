module AsteroidMiner.Data.Game exposing (Command, Game, GroundType(..), Item, Map, Square, SquareType, emptySquare, getBuildingType, getGroundType, isBuildingType, isGroundType, isValid, newBuilding)

import AsteroidMiner.Data.Building as Building exposing (BuildingType(..))
import AsteroidMiner.Data.Comet exposing (Comet)
import AsteroidMiner.Data.Map as Map exposing (SquareType(..))
import AsteroidMiner.Data.Neighborhood as Neighborhood exposing (Neighborhood)
import AsteroidMiner.View.GUI as GUI exposing (Tool(..))
import Grid.Position exposing (Position)


type GroundType
    = Empty
    | Mountain
    | OreGround


newBuilding : Maybe Item -> BuildingType -> Square
newBuilding maybeItem buildingType =
    ( BuildingSquare { counter = 0, sort = buildingType }, maybeItem )


emptySquare : Maybe Item -> Square
emptySquare maybeItem =
    ( GroundSquare Empty, maybeItem )


type alias Command =
    Building.Command BuildingType


type alias Item =
    Never


type alias SquareType =
    Map.SquareType BuildingType GroundType


type alias Square =
    Map.Square BuildingType GroundType Item


getBuildingType : Square -> Maybe BuildingType
getBuildingType square =
    case square of
        ( BuildingSquare { sort }, _ ) ->
            Just sort

        _ ->
            Nothing


isBuildingType : BuildingType -> Square -> Bool
isBuildingType bType =
    getBuildingType
        >> Maybe.map ((==) bType)
        >> Maybe.withDefault False


getGroundType : Square -> Maybe GroundType
getGroundType square =
    case square of
        ( GroundSquare g, _ ) ->
            Just g

        _ ->
            Nothing


isGroundType : GroundType -> Square -> Bool
isGroundType groundType =
    getGroundType
        >> Maybe.map ((==) groundType)
        >> Maybe.withDefault False


type alias Map =
    Map.Map BuildingType GroundType Item


type alias Game =
    { comet : Comet
    , map : Map
    }


isValidMinePos : Neighborhood (Maybe Square) -> Bool
isValidMinePos neigh =
    let
        isConveyorBelt : Maybe Square -> Bool
        isConveyorBelt maybeSquareType =
            case maybeSquareType of
                Just ( BuildingSquare building, _ ) ->
                    case building.sort of
                        Building.ConveyorBelt _ ->
                            True

                        _ ->
                            False

                _ ->
                    False
    in
    [ neigh.up, neigh.left, neigh.right, neigh.down ]
        |> List.any isConveyorBelt


isValid : GUI.Tool -> Position -> Map -> Bool
isValid selected position map =
    case map |> Neighborhood.fromPosition position of
        Ok ( Just square, neigh ) ->
            case square of
                ( GroundSquare Empty, maybeItem ) ->
                    if selected == GUI.Mine then
                        False

                    else
                        True

                ( GroundSquare Mountain, maybeItem ) ->
                    if selected == GUI.Mine then
                        isValidMinePos neigh

                    else
                        False

                _ ->
                    False

        _ ->
            False
