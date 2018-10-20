module CellAutomata exposing (Rule(..),step,Field,RuleState(..),Automata )

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

type alias Automata neighborhood location state
  = { ruleSet:RuleSet neighborhood state
  , symmetry : state -> neighborhood -> Rule neighborhood state-> Bool
  , neighborhoodFunction : Comparable location -> state -> Field (Comparable location) state -> neighborhood
  , order: state -> Int
  , defaultState: state
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


step : Automata neighborhood location state -> Field location state -> Comparable location -> (state -> state)
step ({neighborhoodFunction,symmetry,order,defaultState} as automata) field location =
    let
        neighborhood : neighborhood
        neighborhood =
            field |> neighborhoodFunction location defaultState
        
        (RuleSet ruleSet) = automata.ruleSet
    in
    \state ->
        ruleSet
            |> Dict.get (order state)
            |> Maybe.withDefault []
            |> find (symmetry state neighborhood)
            |> Maybe.map (\(Rule _ _ a) -> a)
            |> Maybe.withDefault state
