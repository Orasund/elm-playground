module FactoryCity.Data.Board exposing
    ( Board
    , columns
    , fromList
    , get
    , getInput
    , getOutput
    , place
    , refill
    , remove
    , rows
    , toList
    , unload
    , values
    )

import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort(..), MovableSort(..))
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position exposing (Position)
import Jsonstore exposing (Json)


columns : Int
columns =
    4


rows : Int
rows =
    columns


type alias Board =
    Grid CellType


get : Position -> Board -> Maybe CellType
get position board =
    case board |> Grid.get position of
        Ok m ->
            m

        Err _ ->
            Nothing


getOutput : Board -> List ContainerSort
getOutput =
    Grid.toList
        >> List.filterMap
            (\( _, v ) ->
                case v.sort of
                    Output ->
                        v.item
                            |> Maybe.map (\i -> CellType.crate i)

                    _ ->
                        Nothing
            )


getInput : Board -> List ContainerSort
getInput =
    Grid.toList
        >> List.filterMap
            (\( _, v ) ->
                case v.sort of
                    Crate i ->
                        Just <| CellType.crate i

                    _ ->
                        Nothing
            )


refill : Board -> Board
refill =
    Grid.map
        (\pos ->
            Maybe.map
                (\({ sort } as cellType) ->
                    case sort of
                        Crate item ->
                            { sort = Crate item, item = Just item }

                        Movable Merger { from, to } ->
                            { sort = CellType.merger to, item = Nothing }

                        Furnace _ ->
                            { sort = Furnace { isWarm = False }, item = Nothing }

                        _ ->
                            cellType
                )
        )


unload : Board -> Board
unload =
    Grid.map
        (\pos ->
            Maybe.map
                (\({ sort } as cellType) ->
                    case sort of
                        Output ->
                            { sort = Output, item = Nothing }

                        _ ->
                            cellType
                )
        )


place : Position -> CellType -> Board -> Board
place position cellType =
    Grid.ignoringErrors <|
        Grid.insert position cellType


remove : Position -> Board -> Board
remove position =
    Grid.ignoringErrors <|
        Grid.remove position



{------------------------
   Json
------------------------}


values : Board -> List CellType
values =
    Grid.values


toList : Board -> List ( Position, CellType )
toList =
    Grid.toList


fromList : List ( Position, CellType ) -> Board
fromList =
    Grid.fromList
        { rows = rows
        , columns = columns
        }
