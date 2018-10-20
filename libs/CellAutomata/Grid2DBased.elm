module CellAutomata.Grid2DBased exposing (noSymmetry,neighborhoodFunction,GridAutomata,Neighborhood,Grid,Location)

import CellAutomata exposing (Field, RuleState(..),Automata,Rule(..))
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


type alias GridAutomata state =
    Automata (Neighborhood state) Location state



neighborhoodFunction :
    Location
    -> state
    -> Field Location state
    -> Neighborhood state
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


noSymmetry : state -> Neighborhood state -> Rule (Neighborhood (RuleState state)) state -> Bool
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