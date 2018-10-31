module RuineJump.Automata exposing (Grid,automata)

import RuineJump.MapElement as MapElement exposing (Block(..))
import CellAutomata.Grid2DBased as Automata exposing (Location,Neighborhood,RuleSet,Rule, GridAutomata)
import Dict exposing (Dict)

type alias Grid = Automata.Grid (Maybe Block)

order: (Maybe Block) -> Int
order maybeBlock = case maybeBlock of
  Nothing -> 0
  Just Dirt -> 1

automata : Dict Int (List (Rule state)) -> GridAutomata (Maybe Block)
automata ruleSet =
  Automata.automata
    { ruleSet = RuleSet ruleSet
    , symmetry = Automata.noSymmetry
    , order=order
    , defaultState=Nothing
    }

