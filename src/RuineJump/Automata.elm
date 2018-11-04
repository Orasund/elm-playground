module RuineJump.Automata exposing (Grid, automata, order)

import CellAutomata.Grid2DBased as Automata exposing (GridAutomata, Location, Neighborhood, Rule, ruleSet)
import Dict exposing (Dict)
import RuineJump.MapElement as MapElement exposing (Block(..))


type alias Grid =
    Automata.Grid Block


order : Maybe Block -> Int
order maybeBlock =
    case maybeBlock of
        Nothing ->
            0

        Just Dirt ->
            1


automata : Dict Int (List (Rule (Maybe Block))) -> GridAutomata (Maybe Block)
automata rules =
    Automata.automata
        { rules = rules
        , symmetry = Automata.noSymmetry
        , order = order
        , defaultState = Nothing
        }

step : GridAutomata (Maybe Block) -> Grid -> Grid
step = Automata.step
