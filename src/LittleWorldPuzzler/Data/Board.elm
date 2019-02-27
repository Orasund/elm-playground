module LittleWorldPuzzler.Data.Board exposing (Board, place)

import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))


type alias Board =
    Grid CellType


place : Position -> CellType -> Board -> Board
place position cellType =
    Grid.ignoringErrors <|
        Grid.insert position cellType
