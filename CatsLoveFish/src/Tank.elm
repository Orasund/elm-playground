module Tank exposing (..)

import Dict exposing (Dict)
import Fish.Common exposing (FishId)
import Set exposing (Set)


type alias FoodId =
    Int


type alias Cell =
    { fish : Set FishId
    , food : Set FoodId
    }


type alias Tank =
    { fishLocations : Dict FishId ( Float, Float )
    , foodLocations : Dict FoodId ( Float, Float )
    , cells : Dict ( Int, Int ) Cell
    }


empty : Tank
empty =
    { fishLocations = Dict.empty
    , foodLocations = Dict.empty
    , cells = Dict.empty
    }


emptyCell : Cell
emptyCell =
    { fish = Set.empty, food = Set.empty }


toPosition : ( Float, Float ) -> ( Int, Int )
toPosition ( x, y ) =
    ( floor x, floor y )


insertFish : ( Float, Float ) -> FishId -> Tank -> Tank
insertFish location fishId tank =
    { tank
        | fishLocations = tank.fishLocations |> Dict.insert fishId location
        , cells =
            tank.cells
                |> Dict.update (toPosition location)
                    (\maybe ->
                        maybe
                            |> Maybe.withDefault emptyCell
                            |> (\cell -> { cell | fish = cell.fish |> Set.insert fishId })
                            |> Just
                    )
    }


insertFood : ( Float, Float ) -> FoodId -> Tank -> Tank
insertFood location foodId tank =
    { tank
        | foodLocations = tank.foodLocations |> Dict.insert foodId location
        , cells =
            tank.cells
                |> Dict.update (toPosition location)
                    (\maybe ->
                        maybe
                            |> Maybe.withDefault emptyCell
                            |> (\cell -> { cell | food = cell.food |> Set.insert foodId })
                            |> Just
                    )
    }


removeFish : FishId -> Tank -> Tank
removeFish fishId tank =
    tank.fishLocations
        |> Dict.get fishId
        |> Maybe.map
            (\location ->
                { tank
                    | fishLocations = tank.fishLocations |> Dict.remove fishId
                    , cells =
                        tank.cells
                            |> Dict.update (toPosition location)
                                (Maybe.map (\cell -> { cell | fish = cell.fish |> Set.remove fishId }))
                }
            )
        |> Maybe.withDefault tank


removeFood : FoodId -> Tank -> Tank
removeFood foodId tank =
    tank.foodLocations
        |> Dict.get foodId
        |> Maybe.map
            (\location ->
                { tank
                    | foodLocations = tank.foodLocations |> Dict.remove foodId
                    , cells =
                        tank.cells
                            |> Dict.update (toPosition location)
                                (Maybe.map (\cell -> { cell | food = cell.food |> Set.remove foodId }))
                }
            )
        |> Maybe.withDefault tank


moveFishTo : ( Float, Float ) -> FishId -> Tank -> Tank
moveFishTo location fishId tank =
    tank.fishLocations
        |> Dict.get fishId
        |> Maybe.map
            (\oldLocation ->
                { tank
                    | fishLocations = tank.fishLocations |> Dict.insert fishId location
                    , cells =
                        tank.cells
                            |> Dict.update (toPosition oldLocation)
                                (Maybe.map (\cell -> { cell | fish = cell.fish |> Set.remove fishId }))
                            |> Dict.update (toPosition location)
                                (\maybe ->
                                    maybe
                                        |> Maybe.withDefault emptyCell
                                        |> (\cell -> { cell | fish = cell.fish |> Set.insert fishId })
                                        |> Just
                                )
                }
            )
        |> Maybe.withDefault tank


fishIds : Tank -> List FishId
fishIds tank =
    tank.fishLocations |> Dict.keys


getFoods : Tank -> Dict FoodId ( Float, Float )
getFoods tank =
    tank.foodLocations


getFish : Tank -> Dict FishId ( Float, Float )
getFish tank =
    tank.fishLocations


getFishLocation : FishId -> Tank -> Maybe ( Float, Float )
getFishLocation fishId tank =
    tank.fishLocations |> Dict.get fishId


getCell : ( Float, Float ) -> Tank -> Cell
getCell location tank =
    tank.cells
        |> Dict.get (toPosition location)
        |> Maybe.withDefault emptyCell


getFishCell : FishId -> Tank -> Cell
getFishCell fishId tank =
    getFishLocation fishId tank
        |> Maybe.map (\location -> getCell location tank)
        |> Maybe.withDefault emptyCell
