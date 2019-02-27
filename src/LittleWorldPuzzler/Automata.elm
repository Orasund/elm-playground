module LittleWorldPuzzler.Automata exposing (step)

import CellAutomata exposing (Automata, Order, Rule)
import Dict exposing (Dict)
import LittleWorldPuzzler.Automata.Neighborhood as Neighborhood
import LittleWorldPuzzler.Automata.Rule as Rule
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType)


order : Order CellType
order =
    Maybe.map CellType.toInt
        >> Maybe.withDefault 0


rules : List (Rule CellType)
rules =
    CellType.list
        |> List.map Rule.rules
        |> List.concat


automata : Automata CellType
automata =
    CellAutomata.automata
        Neighborhood.fullSymmetry
        order
        rules


step : Dict ( Int, Int ) CellType -> ( Int, Int ) -> Maybe CellType -> Maybe CellType
step =
    CellAutomata.step
        automata
