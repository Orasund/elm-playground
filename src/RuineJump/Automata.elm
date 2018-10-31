module RuineJump.Automata exposing (Grid,automata)

import RuineJump.MapElement as MapElement exposing (Block(..))
import CellAutomata.Grid2DBased as Automata exposing (Location,Neighborhood,RuleSet, GridAutomata)

type alias Grid = Automata.Grid Block

automata : { ruleSet:RuleSet Block
      , order: Block -> Int
      , defaultState: Block
      } -> GridAutomata Block
automata {ruleSet,order,defaultState} =
  Automata.automata
    { ruleSet = ruleSet
    , symmetry = Automata.noSymmetry
    , order=order
    , defaultState=defaultState
    }

