module FactoryCity.Automata exposing
    ( ListRule
    , automata
    , step
    )

import CellAutomata exposing (Order, Rule)
import Dict exposing (Dict)
import FactoryCity.Data.CellType as CellType exposing (CellType)


type alias ListRule =
    List (Rule CellType)


order : Order CellType ( String, String )
order =
    Maybe.map CellType.toString
        >> Maybe.withDefault ( "", "" )


automata : List (Rule CellType) -> CellAutomata.Automata CellType ( String, String )
automata =
    CellAutomata.automata
        CellAutomata.noSymmetry
        order


step : CellAutomata.Automata CellType ( String, String ) -> Dict ( Int, Int ) CellType -> ( Int, Int ) -> Maybe CellType -> Maybe CellType
step =
    CellAutomata.step
