module FactoryCity.Automata.Neighborhood exposing (set)

import CellAutomata exposing (Neighborhood, RuleExpression(..), Symmetry, anyNeighborhood)
import Dict
import FactoryCity.Data.CellType as CellType exposing (CellType)
import Grid.Direction exposing (Direction(..))


set : Direction -> RuleExpression (Maybe state) -> Neighborhood (RuleExpression (Maybe state)) -> Neighborhood (RuleExpression (Maybe state))
set dir exp neighborhood =
    case dir of
        Up ->
            { neighborhood | west = exp }

        Right ->
            { neighborhood | south = exp }

        Down ->
            { neighborhood | east = exp }

        Left ->
            { neighborhood | north = exp }
