module RuineJump.MapSegment exposing (floorGenerator)

import CellAutomata.Grid2DBased exposing (Location)
import Dict exposing (Dict)
import Natural exposing (Natural16(..))
import Random exposing (Generator)
import RuineJump.Automata as Automata exposing (Grid, automata)
import CellAutomata.Grid2DBased exposing (Rule,rule)
import CellAutomata exposing (RuleState(..))
import RuineJump.Rules as Rules
import RuineJump.MapElement as MapElement exposing (Block(..), MapElement(..))


floorGenerator : Generator (Dict Location MapElement)
floorGenerator =
    let
        width : Int
        width = 20

        height : Int
        height = 8

        newSeed : Int -> Location -> Random.Seed
        newSeed seed ( x, y ) =
            Random.initialSeed (seed + x * width + y)

        toSegment : Int -> Grid -> Dict Location MapElement
        toSegment seed =
            Dict.map
                (\pos block ->
                    BlockElement block <| Tuple.first <| Random.step MapElement.nat16Generator <| newSeed seed <| pos
                )

        build : List (Maybe Block) -> List ( Location, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x, -2 ), elem )))
                >> List.filterMap identity
        
        step : Dict Int (List (Rule (Maybe Block))) -> Grid -> Grid
        step rules grid = List.range 0 width
            |>List.foldl
                (\x partList ->
                    List.range (-1*height) 0
                    |> List.foldl
                        (\y list ->
                            let
                                pos : Location
                                pos = (x,y)
                                
                                elem : Maybe Block
                                elem = grid |> Dict.get pos
                            in
                            case Automata.step (automata rules) grid pos elem of
                                Just a ->
                                    (pos,a) :: list
                                Nothing ->
                                    list
                        )
                    partList
                )
                []
            |> Dict.fromList
        
        repeat : Int -> (a -> a) -> a -> a
        repeat num fun dict =
            List.range 0 (num-1)
            |> List.foldl (always <| fun) dict
        
    in
    Random.map2
        (\seed ->
            build
                >> (\list ->
                        List.concat
                            [ list
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, -1 ), elem ))
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, 0 ), elem ))
                            ]
                   )
                >> Dict.fromList
                >> repeat 2 (step Rules.placeDirt)
                >> repeat 2 (step Rules.placeGrass)
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list 20 (Random.weighted ( 3, Nothing ) [ ( 10, Just Dirt ) ]))
