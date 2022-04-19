module LittleWorldPuzzler.Data.Board exposing
    ( Board
    , columns
    , fromList
    , jsonTuple
    , place
    , rows
    , toList
    , values
    )

import Grid.Bordered as Grid exposing (Grid)
import Jsonstore exposing (Json)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import Position


columns : Int
columns =
    4


rows : Int
rows =
    columns


type alias Board =
    Grid CellType


place : ( Int, Int ) -> CellType -> Board -> Board
place position cellType =
    Grid.ignoringErrors <|
        Grid.insert position cellType



{------------------------
   Json
------------------------}


jsonTuple : Json ( ( Int, Int ), CellType )
jsonTuple =
    Jsonstore.object (\x y value -> ( ( x, y ), value ))
        |> Jsonstore.with "x" Jsonstore.int (\( ( x, _ ), _ ) -> x)
        |> Jsonstore.with "y" Jsonstore.int (\( ( _, y ), _ ) -> y)
        |> Jsonstore.with "value" CellType.json (\( _, value ) -> value)
        |> Jsonstore.toJson


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
