module CellAutomata exposing (Rule(..),step,Field,RuleSet(..),RuleState(..),Automata,Symmetry, NeighborhoodFunction)

import Dict exposing (Dict)


type alias Comparable comparable =
    comparable


type alias Field location state =
    Dict (Comparable location) state


type RuleSet neighborhood state
    = RuleSet (Dict Int (List (Rule neighborhood state)))

type RuleState state
    = Exactly state
    | Anything

type Rule neighborhood state
    = Rule neighborhood state state

type alias Symmetry neighborhood ruleNeighborhood state
    = state -> neighborhood -> Rule ruleNeighborhood state -> Bool

type alias NeighborhoodFunction location neighborhood state
    = Comparable location -> Field (Comparable location) state -> neighborhood

type alias Automata neighborhood ruleNeighborhood location state
  = { ruleSet: RuleSet ruleNeighborhood (Maybe state)
  , symmetry : Symmetry neighborhood ruleNeighborhood (Maybe state)
  , neighborhoodFunction : NeighborhoodFunction location neighborhood state
  , order: (Maybe state) -> Int
  }

find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


step : Automata neighborhood ruleNeighborhood location state -> Field location state -> (location-> (Maybe state) -> Maybe state)
step ({neighborhoodFunction,symmetry,order} as automata) field=
    \location state ->
        let
            neighborhood : neighborhood
            neighborhood =
                field |> neighborhoodFunction location
            
            (RuleSet ruleSet) = automata.ruleSet
        in
        ruleSet
            |> Dict.get (order state)
            |> Maybe.withDefault []
            |> find (symmetry state neighborhood)
            |> Maybe.map (\(Rule _ _ a) -> a)
            |> Maybe.withDefault state
    