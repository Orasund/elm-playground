module CellAutomata.Grid2DBased exposing (step,noSymmetry,automata,neighborhoodFunction,RuleSet,GridAutomata,Neighborhood,Grid,Location)

import CellAutomata exposing (Field,Symmetry,Automata,Rule(..),RuleState(..),NeighborhoodFunction,Symmetry)
import Dict


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

type alias RuleSet state = 
    CellAutomata.RuleSet (Neighborhood (RuleState state)) state

type alias GridAutomata state =
    Automata (Neighborhood state) (Neighborhood (RuleState state)) Location state

automata : { ruleSet:RuleSet state
  , symmetry : Symmetry (Neighborhood state) (Neighborhood (RuleState state)) state
  , order: state -> Int
  , defaultState: state
  } -> GridAutomata state
automata {ruleSet,symmetry,order,defaultState}=
    { ruleSet=ruleSet
  , symmetry = symmetry
  , neighborhoodFunction = neighborhoodFunction
  , order= order
  , defaultState= defaultState
  }


neighborhoodFunction : NeighborhoodFunction Location (Neighborhood state) state
neighborhoodFunction ((x,y) as location) defaultState field =
    { north = field |> Dict.get (x, y - 1) |> Maybe.withDefault defaultState
    , northEast = field |> Dict.get (x + 1, y - 1) |> Maybe.withDefault defaultState
    , east = field |> Dict.get (x + 1,y) |> Maybe.withDefault defaultState
    , southEast = field |> Dict.get (x + 1,y + 1) |> Maybe.withDefault defaultState
    , south = field |> Dict.get (x,y + 1) |> Maybe.withDefault defaultState
    , southWest = field |> Dict.get (x - 1, y + 1) |> Maybe.withDefault defaultState
    , west = field |> Dict.get (x - 1,y) |> Maybe.withDefault defaultState
    , northWest = field |> Dict.get (x - 1,y - 1) |> Maybe.withDefault defaultState
    }


noSymmetry : Symmetry (Neighborhood state) (Neighborhood (RuleState state)) state
noSymmetry state neighborhood (Rule ruleNeighborhood ruleState _) =
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

step : GridAutomata state -> Grid state -> Location -> (state -> state)
step = CellAutomata.step