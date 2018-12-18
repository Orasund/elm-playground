module CellAutomata.LifeLike exposing (automata,step,AliveNeighbors,Automata,Location,Grid,State(..))

{-| Conway's Game of Life is the most famous cell automata.
This module was created to give a smooth introduction to the main module,
but also because for a lot of usecases this simpler version is already enough.

In this module, a few assumptions about the automata where made:
* The cells are organiced in a two dimensional grid.
* Each cell can have on of two states: Dead(Nothing) or Alive(Just Alive)
* Each cell has Eight neighbors
* A Rule can only take the amount of alive neighbored cells into account. (Not the pattern)
*Note for the advanced:* This last assumption can be ignored by using
the automataWithoutSymmetry(the exact pattern must the matched) 
or automataWithCustomSymmetry(for example mirrored or rotational symmetry).

# Basic types
@docs Grid,State,Location

# Automata
@docs AliveNeighbors,step,automata

# For the Advanced
The remaining documentation is for more advanced user.
@docs Automata
-}

import Dict exposing (Dict)
import CellAutomata

{-| The type of a cell will be of (Maybe State). This is to make clear, that
If not specified otherwise, a cell will be "Nothing".
In this module there is just one other state: "Alive".
If you want to add more states, then this you should go to the main module.
-}
type State = Alive

order : Maybe State -> Int
order maybeState = case maybeState of
  Nothing -> 0
  Just Alive -> 1

{-| The location is the unique identifier for any cell.
For our purpose we use (x,y)-coordinates.

*Note for the advanced:* The south of (0,0) is (0,y) while the north is (0,-y).
This might be of inportance as soon as you start playing around with symmetries.
-}
type alias Location =
    (Int,Int)


{-| The grid is the "model" of this module. You might want to write your own view function
for it or else you can't see what the automata has done.

In your head you should think of this as a grid, there some cells may be filled.
Filled cells have the value "Just Alive" while empty cells have the value "Nothing".
This is why we can represent the grid as a dictionary.
-}
type alias Grid =
    Dict Location State

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

{-| This type specifies how many neighbors may be alive.
-}
type AliveNeighbors =
    AllDead
    | OneAlive
    | TwoAlive
    | ThreeAlive
    | FourAlive
    | FiveAlive
    | SixAlive
    | SevenAlive
    | EightAlive

createNeighborhood : AliveNeighbors -> Neighborhood (Maybe State)
createNeighborhood neighbors = case neighbors of
    AllDead ->
      { north = Nothing
      , northEast = Nothing
      , east = Nothing
      , southEast = Nothing
      , south = Nothing
      , southWest = Nothing
      , west = Nothing
      , northWest = Nothing
      }
    OneAlive ->
      { north = Just Alive
      , northEast = Nothing
      , east = Nothing
      , southEast = Nothing
      , south = Nothing
      , southWest = Nothing
      , west = Nothing
      , northWest = Nothing
      }
    TwoAlive ->
      { north = Just Alive
      , northEast = Just Alive
      , east = Nothing
      , southEast = Nothing
      , south = Nothing
      , southWest = Nothing
      , west = Nothing
      , northWest = Nothing
      }
    ThreeAlive ->
      { north = Just Alive
      , northEast = Just Alive
      , east = Just Alive
      , southEast = Nothing
      , south = Nothing
      , southWest = Nothing
      , west = Nothing
      , northWest = Nothing
      }
    FourAlive ->
      { north = Just Alive
      , northEast = Just Alive
      , east = Just Alive
      , southEast = Just Alive
      , south = Nothing
      , southWest = Nothing
      , west = Nothing
      , northWest = Nothing
      }
    FiveAlive ->
      { north = Just Alive
      , northEast = Just Alive
      , east = Just Alive
      , southEast = Just Alive
      , south = Just Alive
      , southWest = Nothing
      , west = Nothing
      , northWest = Nothing
      }
    SixAlive ->
      { north = Just Alive
      , northEast = Just Alive
      , east = Just Alive
      , southEast = Just Alive
      , south = Just Alive
      , southWest = Just Alive
      , west = Nothing
      , northWest = Nothing
      }
    SevenAlive ->
      { north = Just Alive
      , northEast = Just Alive
      , east = Just Alive
      , southEast = Just Alive
      , south = Just Alive
      , southWest = Just Alive
      , west = Just Alive
      , northWest = Nothing
      }
    EightAlive ->
      { north = Just Alive
      , northEast = Just Alive
      , east = Just Alive
      , southEast = Just Alive
      , south = Just Alive
      , southWest = Just Alive
      , west = Just Alive
      , northWest = Just Alive
      }

type alias Rule
    = {from:(Maybe State),neighbors:(Neighborhood (Maybe State)),to:(Maybe State)}

type alias Symmetry
    = (Maybe State) -> Neighborhood (Maybe State) -> Rule -> Bool

fullSymmetry : Symmetry
fullSymmetry state neighborhood {from,neighbors} =
    let
      sum : Neighborhood (Maybe State) -> Int
      sum x = 
        [ x.north,x.northEast,x.east,x.southEast
        , x.south,x.southWest,x.west,x.northWest
        ]
        |> List.foldl
          (\a s -> s + (order a))
          0
    in
    (state == from)
    && sum neighborhood == sum neighbors

noSymmetry : Symmetry
noSymmetry state neighborhood {from,neighbors} =
    (state == from)
    && neighborhood.north == neighbors.north
    && neighborhood.northEast == neighbors.northEast
    && neighborhood.east == neighbors.east
    && neighborhood.southEast == neighbors.southEast
    && neighborhood.south == neighbors.south
    && neighborhood.southWest == neighbors.southWest
    && neighborhood.west == neighbors.west
    && neighborhood.northWest == neighbors.northWest

type alias NeighborhoodFunction
    = Location -> Grid -> Neighborhood (Maybe State)

neighborhoodFunction : NeighborhoodFunction
neighborhoodFunction ((x,y) as location) grid =
    { north = grid |> Dict.get (x, y - 1)
    , northEast = grid |> Dict.get (x + 1, y - 1)
    , east = grid |> Dict.get (x + 1,y)
    , southEast = grid |> Dict.get (x + 1,y + 1)
    , south = grid |> Dict.get (x,y + 1)
    , southWest = grid |> Dict.get (x - 1, y + 1)
    , west = grid |> Dict.get (x - 1,y)
    , northWest = grid |> Dict.get (x - 1,y - 1)
    }

type alias Automata
    = { ruleSet: CellAutomata.RuleSet (Neighborhood (Maybe State)) (Maybe State)
    , symmetry : Symmetry
    , neighborhoodFunction : NeighborhoodFunction
    , order: Maybe State -> Int
    }

{-| The Automata type can be seen as a config type.
Its stores all information to specify the behaviour of a cell automata.
Sometimes more then one automata should act on to the same Grid.
For this reason it is its own type.

The input is a list of rules.
The rules for conways game of life are as follows:
    [ {from = Just Alive, neighbors = AllDead, to = Nothing}
    , {from = Just Alive, neighbors = OneAlive, to = Nothing}
    , {from = Just Alive, neighbors = FourAlive, to = Nothing}
    , {from = Just Alive, neighbors = FiveAlive, to = Nothing}
    , {from = Just Alive, neighbors = SixAlive, to = Nothing}
    , {from = Just Alive, neighbors = SevenAlive, to = Nothing}
    , {from = Just Alive, neighbors = EightAlive, to = Nothing}
    , {from = Nothing, neighbors = ThreeAlive, to = Just Alive}
    ]

*Note for the advanced:*
The main module introduces so called RuleExpressions, with which the same
rules could be expressed in the following way:
    [ {from = Just Alive, neighbors = Exactly <| TwoAlive, to = Just Alive}
    , {from = Just Alive, neighbors = Exactly <| ThreeAlive, to = Just Alive}
    , {from = Just Alive, neighbors = Anything, to = Nothing}
    , {from = Nothing, neighbors = Exactly <| ThreeAlive, to = Just Alive}
    ]
-}
automata : List {from:(Maybe State),neighbors:AliveNeighbors,to:(Maybe State)} -> Automata
automata =
    List.map
          (\{from,neighbors,to} -> 
            {from=from,neighbors = neighbors |> createNeighborhood,to=to}
          )
      >> automataWithCustomSymmetry fullSymmetry

automataWithoutSymmetry : List Rule -> Automata
automataWithoutSymmetry = automataWithCustomSymmetry noSymmetry

automataWithCustomSymmetry : Symmetry -> List Rule -> Automata
automataWithCustomSymmetry symmetry listOfRules =
  { ruleSet =
      listOfRules
        |> CellAutomata.ruleSet order
  , symmetry = symmetry
  , neighborhoodFunction = neighborhoodFunction
  , order = order
  }

{-| This is the main function.
It has a wierd type, but thats because it is meant to be used with Dict.map:
    List.range 0 10
    |> List.foldl
        (always Dict.map (step automata grid))
        grid
-}
step : Automata -> Grid -> (Location -> Maybe State -> Maybe State)
step = CellAutomata.step