module Tank exposing (..)

import Dict exposing (Dict)
import Fish exposing (FishId)
import Set exposing (Set)


type alias FoodId =
    Int


type alias Cell =
    { fish : Set FishId }


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
    { fish = Set.empty }


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


getFishLocation : FishId -> Tank -> Maybe ( Float, Float )
getFishLocation fishId tank =
    tank.fishLocations |> Dict.get fishId
