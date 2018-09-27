module CellAutomata.GridBased exposing (step)

import CellAutomata exposing (Field, RuleState,Automata,Rule)
import Dict


type alias Location =
    { x : Int, y : Int }


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
    -> Field Location state
    -> Neighborhood (Maybe state)
neighborhoodFunction ({ x, y } as location) field =
    { north = field |> Dict.get { location | y = y - 1 }
    , northEast = field |> Dict.get { y = y - 1, x = x + 1 }
    , east = field |> Dict.get { location | x = x + 1 }
    , southEast = field |> Dict.get { y = y + 1, x = x + 1 }
    , south = field |> Dict.get { location | y = y + 1 }
    , southWest = field |> Dict.get { x = x - 1, y = y + 1 }
    , west = field |> Dict.get { location | x = x - 1 }
    , northWest = field |> Dict.get { y = y - 1, x = x - 1 }
    }


noSymmetry : Maybe state -> Neighborhood (Maybe state) -> Rule (Neighborhood (RuleState state)) (Maybe state) -> Bool
noSymmetry maybeState neighborhood (Rule ruleNeighborhood ruleState) =
    (maybeState == ruleState)
        (ruleNeighborhood.north
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
