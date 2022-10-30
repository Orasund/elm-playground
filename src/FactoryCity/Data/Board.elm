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
import FactoryCity.Data.Item exposing (Item(..))
import Grid.Bordered as Grid exposing (Grid)


columns : Int
columns =
    4


rows : Int
rows =
    columns


type alias Board =
    Grid CellType


get : ( Int, Int ) -> Board -> Maybe CellType
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


getInput : Item -> Board -> List ContainerSort
getInput item =
    Grid.toList
        >> List.filterMap
            (\( _, v ) ->
                case v.sort of
                    Crate i ->
                        if i == item then
                            Nothing

                        else
                            Just <| CellType.crate i

                    _ ->
                        Nothing
            )


refill : Board -> Board
refill =
    Grid.map
        (\_ ->
            Maybe.map
                (\({ sort } as cellType) ->
                    case sort of
                        Crate item ->
                            { sort = Crate item, item = Just item }

                        Movable Merger { to } ->
                            { sort = CellType.merger to, item = Nothing }

                        Machine machineSort _ ->
                            { sort = Machine machineSort False, item = Nothing }

                        _ ->
                            { sort = cellType.sort, item = Nothing }
                )
        )


unload : Board -> Board
unload =
    Grid.map
        (\_ ->
            Maybe.map
                (\({ sort } as cellType) ->
                    case sort of
                        Output ->
                            { sort = Output, item = Nothing }

                        _ ->
                            cellType
                )
        )


place : ( Int, Int ) -> CellType -> Board -> Board
place position cellType =
    Grid.ignoringErrors <|
        Grid.insert position cellType


remove : ( Int, Int ) -> Board -> Board
remove position =
    Grid.ignoringErrors <|
        Grid.remove position



{------------------------
   Json
------------------------}


values : Board -> List CellType
values =
    Grid.values


toList : Board -> List ( ( Int, Int ), CellType )
toList =
    Grid.toList


fromList : List ( ( Int, Int ), CellType ) -> Board
fromList =
    Grid.fromList
        { rows = rows
        , columns = columns
        }
