module RuineJump.MapSegment exposing (floorGenerator)

import Dict exposing (Dict)
import Natural exposing (Natural16(..))
import Random exposing (Generator)
import RuineJump.MapElement as MapElement exposing (MapElement(..),Block(..))
import RuineJump.Automata as Automata exposing (Grid,automata)
import CellAutomata.Grid2DBased exposing (Location)

floorGenerator : Generator (Dict Location MapElement)
floorGenerator =
    let
        newSeed : Int -> Location -> Random.Seed
        newSeed seed ( x, y ) =
            Random.initialSeed (seed + x * 20 + y)

        toSegment : Int -> Grid -> Dict Location MapElement
        toSegment seed =
            Dict.map
                (\ pos block  ->
                    BlockElement block <| Tuple.first <| Random.step MapElement.nat16Generator <| newSeed seed <| pos
                )
        
        build : List (Maybe Block) -> List (Location,Block)
        build =
          List.indexedMap (\x -> Maybe.map (\elem -> (( x, -1 ), elem) ))
                >> List.filterMap identity
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
                {- >> (\list -> 
                  list |> List.map (\(location, block) -> Automata.step list location block)
                )
                 -}
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list 20 (Random.weighted ( 10, Nothing ) [ ( 10, Just Dirt ) ]))
