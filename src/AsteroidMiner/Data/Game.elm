module AsteroidMiner.Data.Game exposing (Game, emptySquare, getBuildingType, getGroundType, isBuildingType, isGroundType, isValid, newBuilding, solveConflict, updateBuilding)

import AsteroidMiner.Building as Building exposing (BuildingType(..))
import AsteroidMiner.Building.ColoredConveyorBelt as ColoredConveyorBelt
import AsteroidMiner.Building.Container as Container
import AsteroidMiner.Building.ConveyorBelt as ConveyorBelt
import AsteroidMiner.Building.Mine as Mine
import AsteroidMiner.Data.Comet exposing (Comet)
import AsteroidMiner.Data.Map as Map exposing (GroundType(..), Item, Map, Neighborhood, Square)
import AsteroidMiner.Lib.Map as Map exposing (SquareType(..))
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import AsteroidMiner.View.GUI as GUI
import Grid.Position exposing (Position)


type alias Game =
    { comet : Comet
    , map : Map
    }


solveConflict : BuildingType -> Neighborhood -> Item -> { item : Item, value : Int } -> Bool
solveConflict sort neigh =
    case sort of
        ColoredConveyorBelt _ _ ->
            ColoredConveyorBelt.canStore neigh

        ConveyorBelt _ ->
            ConveyorBelt.canStore neigh

        Mine ->
            Mine.canStore neigh

        Container ->
            Container.canStore neigh


updateBuilding : BuildingType -> ({ value : Int, item : Maybe Item } -> Neighborhood -> Map.Command)
updateBuilding sort =
    case sort of
        ColoredConveyorBelt color dir ->
            always <| ColoredConveyorBelt.update color dir

        ConveyorBelt code ->
            always <| ConveyorBelt.update code

        Mine ->
            Mine.update

        Container ->
            always <| Container.update


newBuilding : Maybe Item -> BuildingType -> Square
newBuilding maybeItem buildingType =
    let
        value : Int
        value =
            case buildingType of
                Building.Mine ->
                    128

                _ ->
                    0
    in
    ( BuildingSquare { value = value, sort = buildingType }, maybeItem )


emptySquare : Maybe Item -> Square
emptySquare maybeItem =
    ( GroundSquare Empty, maybeItem )


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


isValidMinePos : Neighborhood.Neighborhood (Maybe Square) -> Bool
isValidMinePos neigh =
    [ neigh.up, neigh.left, neigh.right, neigh.down ]
        |> List.any
            (Maybe.map
                (Tuple.first
                    >> ((/=) <| GroundSquare Mountain)
                )
                >> Maybe.withDefault False
            )


isValid : GUI.Tool -> Position -> Map -> Bool
isValid selected position map =
    case map |> Neighborhood.fromPosition position of
        Ok ( Just square, neigh ) ->
            case square of
                ( GroundSquare Empty, _ ) ->
                    if selected == GUI.Mine then
                        False

                    else
                        True

                ( GroundSquare Mountain, _ ) ->
                    if selected == GUI.Mine then
                        isValidMinePos neigh

                    else
                        False

                _ ->
                    False

        _ ->
            False
