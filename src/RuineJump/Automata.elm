module RuineJump.Automata exposing (Grid, automata, order, step)

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

        Just Grass ->
            2

        Just Stone ->
            3


automata : Dict Int (List (Rule (Maybe Block))) -> GridAutomata Block
automata rules =
    Automata.automata
        { rules = rules
        , symmetry = Automata.noSymmetry
        , order = order
        }


step : GridAutomata Block -> Grid -> (Location -> Maybe Block -> Maybe Block)
step =
    Automata.step
