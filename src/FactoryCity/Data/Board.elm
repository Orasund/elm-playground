module FactoryCity.Data.Board exposing
    ( Board
    , columns
    , fromList
    , place
    , rows
    , toList
    , values
    )

import FactoryCity.Data.CellType as CellType exposing (CellType)
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


place : Position -> CellType -> Board -> Board
place position cellType =
    Grid.ignoringErrors <|
        Grid.insert position cellType



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
