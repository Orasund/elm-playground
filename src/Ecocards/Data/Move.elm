module Ecocards.Data.Move exposing (Move, getSubset, toggle)

import Bag exposing (Bag)
import Dict exposing (Dict)
import List.Extra as List
import Set exposing (Set)
import Set.Extra as Set


type alias Move =
    { animalId : Int
    , selected : Set Int
    , played : Set Int
    }


toggle : Int -> Move -> Move
toggle id move =
    { move
        | selected = move.selected |> Set.toggle id
    }


getSubset : Bag Int -> Dict Int { animal | strength : Int } -> Set Int
getSubset bag a =
    bag
        |> Bag.toList
        |> List.foldl
            (\( s, amount ) acc ->
                List.repeat amount s
                    |> List.foldl
                        (\strength ( ids, out ) ->
                            case ids |> Dict.get strength of
                                Just (id :: rem) ->
                                    ( ids |> Dict.insert strength rem
                                    , id :: out
                                    )

                                Just [] ->
                                    ( ids, out )

                                Nothing ->
                                    ( ids, out )
                        )
                        acc
            )
            ( a
                |> Dict.toList
                |> List.map (\( id, { strength } ) -> ( id, strength ))
                |> List.gatherEqualsBy (\( _, strength ) -> strength)
                |> List.map
                    (\( ( id, strength ), list ) ->
                        ( strength
                        , id :: (list |> List.map Tuple.first)
                        )
                    )
                |> Dict.fromList
            , []
            )
        |> Tuple.second
        |> Set.fromList
