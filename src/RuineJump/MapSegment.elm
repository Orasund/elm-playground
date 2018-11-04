module RuineJump.MapSegment exposing (floorGenerator)

import CellAutomata.Grid2DBased exposing (Location)
import Dict exposing (Dict)
import Natural exposing (Natural16(..))
import Random exposing (Generator)
import RuineJump.Automata as Automata exposing (Grid, automata)
import CellAutomata.Grid2DBased exposing (Rule,rule)
import CellAutomata exposing (RuleState(..))
import RuineJump.MapElement as MapElement exposing (Block(..), MapElement(..))


floorGenerator : Generator (Dict Location MapElement)
floorGenerator =
    let
        newSeed : Int -> Location -> Random.Seed
        newSeed seed ( x, y ) =
            Random.initialSeed (seed + x * 20 + y)

        toSegment : Int -> Grid -> Dict Location MapElement
        toSegment seed =
            Dict.map
                (\pos block ->
                    BlockElement block <| Tuple.first <| Random.step MapElement.nat16Generator <| newSeed seed <| pos
                )

        build : List (Maybe Block) -> List ( Location, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x, -1 ), elem )))
                >> List.filterMap identity

        rules : Dict Int (List (Rule (Maybe Block)))
        rules =
            Dict.empty
                |> Dict.insert (Nothing |> Automata.order)
                    [rule
                        { from = Nothing
                        , neighbors =
                            { north = Anything
                            , northEast = Anything
                            , east = Anything
                            , southEast = Exactly <| Just Dirt
                            , south = Exactly <| Just Dirt
                            , southWest = Exactly <| Just Dirt
                            , west = Anything
                            , northWest = Anything
                            }
                        , to = Just Dirt
                        }
                    ]
        
    in
    Random.map2
        (\seed ->
            build
                >> (\list ->
                        List.concat
                            [ list
                            , List.repeat 20 Dirt
                                |> List.indexedMap (\x elem -> ( ( x, 0 ), elem ))
                            ]
                   )
                >> Dict.fromList
                >> Automata.step (automata rules)
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list 20 (Random.weighted ( 10, Nothing ) [ ( 10, Just Dirt ) ]))
