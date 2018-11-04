module CellAutomata.Grid2DBased exposing (step,noSymmetry,automata,rule,neighborhoodFunction,ruleSet,Rule,RuleSet,GridAutomata,Neighborhood,Grid,Location)

import CellAutomata exposing (Field,Symmetry,Automata,RuleState(..),NeighborhoodFunction,Symmetry)
import Dict exposing (Dict)


type alias Location =
    (Int,Int)


type alias Grid state =
    Field Location state


type alias Neighborhood state =
    { north : state
    , northEast : state
    , east : state
    , southEast : state
    , south : state
    , southWest : state
    , west : state
    , northWest : state
    }

type alias Rule state =
    CellAutomata.Rule (Neighborhood (RuleState state)) state 

rule : {from:state,neighbors:(Neighborhood (RuleState state)),to:state} -> Rule state
rule {from,neighbors,to}=
    CellAutomata.Rule neighbors from to

type alias RuleSet state = 
    CellAutomata.RuleSet (Neighborhood (RuleState state)) state

ruleSet : Dict Int (List (Rule state)) -> RuleSet state
ruleSet = CellAutomata.RuleSet

type alias GridAutomata state =
    Automata (Neighborhood (Maybe state)) (Neighborhood (RuleState (Maybe state))) Location state

automata : { rules:Dict Int (List (Rule (Maybe state)))
  , symmetry : Symmetry (Neighborhood (Maybe state)) (Neighborhood (RuleState (Maybe state))) (Maybe state)
  , order: (Maybe state) -> Int
  } -> GridAutomata state
automata {rules,symmetry,order}=
    { ruleSet=ruleSet rules
  , symmetry = symmetry
  , neighborhoodFunction = neighborhoodFunction
  , order= order
  }


neighborhoodFunction : NeighborhoodFunction Location (Neighborhood (Maybe state)) state
neighborhoodFunction ((x,y) as location) field =
    { north = field |> Dict.get (x, y - 1)
    , northEast = field |> Dict.get (x + 1, y - 1)
    , east = field |> Dict.get (x + 1,y)
    , southEast = field |> Dict.get (x + 1,y + 1)
    , south = field |> Dict.get (x,y + 1)
    , southWest = field |> Dict.get (x - 1, y + 1)
    , west = field |> Dict.get (x - 1,y)
    , northWest = field |> Dict.get (x - 1,y - 1)
    }


noSymmetry : Symmetry (Neighborhood state) (Neighborhood (RuleState state)) state
noSymmetry state neighborhood (CellAutomata.Rule ruleNeighborhood ruleState _) =
        (state == ruleState)
        && (ruleNeighborhood.north
                == Anything
                || Exactly neighborhood.north
                == ruleNeighborhood.north
        )
        && (ruleNeighborhood.northEast
                == Anything
                || Exactly neighborhood.northEast
                == ruleNeighborhood.northEast
           )
        && (ruleNeighborhood.east
                == Anything
                || Exactly neighborhood.east
                == ruleNeighborhood.east
           )
        && (ruleNeighborhood.southEast
                == Anything
                || Exactly neighborhood.southEast
                == ruleNeighborhood.southEast
           )
        && (ruleNeighborhood.south
                == Anything
                || Exactly neighborhood.south
                == ruleNeighborhood.south
           )
        && (ruleNeighborhood.southWest
                == Anything
                || Exactly neighborhood.southWest
                == ruleNeighborhood.southWest
           )
        && (ruleNeighborhood.west
                == Anything
                || Exactly neighborhood.west
                == ruleNeighborhood.west
           )
        && (ruleNeighborhood.northWest
                == Anything
                || Exactly neighborhood.northWest
                == ruleNeighborhood.northWest
           )

step : GridAutomata state -> Grid state -> (Location-> (Maybe state) -> Maybe state)
step = CellAutomata.step