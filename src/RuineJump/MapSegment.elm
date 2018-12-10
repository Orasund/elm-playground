module RuineJump.MapSegment exposing (floorGenerator,parkourGenerator)

import CellAutomata.Grid2DBased exposing (Location)
import Dict exposing (Dict)
import Natural exposing (Natural16(..))
import Random exposing (Generator)
import RuineJump.Automata as Automata exposing (Grid, automata)
import CellAutomata.Grid2DBased exposing (Rule,rule)
import CellAutomata exposing (RuleState(..))
import RuineJump.Rules as Rules
import RuineJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuineJump.Config as Config

width : Int
width = Config.width

height : Int
height = Config.sectionHeight

newSeed : Int -> Location -> Random.Seed
newSeed seed ( x, y ) =
    Random.initialSeed (seed + x * width + y)

toSegment : Int -> Grid -> Dict Location MapElement
toSegment seed =
    Dict.map
        (\pos block ->
            BlockElement block <| Tuple.first <| Random.step (Random.int 0 Random.maxInt) <| newSeed seed <| pos
        )

step : Int -> Dict Int (List (Rule (Maybe Block))) -> Grid -> Grid
step yOffset rules grid = 
    List.range 0 width
    |>List.foldl
        (\x partList ->
            List.range (-1*height-yOffset) (-yOffset)
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

parkourGenerator : Int -> Generator (Dict Location MapElement)
parkourGenerator level = 
    let
        yOffset : Int
        yOffset = level*height

        build : List (Maybe Block) -> List ( Location, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x |> modBy width, -1-yOffset-2*(x//width) ), elem )))
                >> List.filterMap identity
    in
    Random.map2
        (\seed ->
            build
                >> (\list ->
                        List.concat
                            [ list
                            , List.repeat height Stone
                                |> List.indexedMap (\x elem -> ( ( 0, -x-yOffset ), elem ))
                            , List.repeat height Stone
                                |> List.indexedMap (\x elem -> ( ( width-1, -x-yOffset ), elem ))
                            ]
                    )
                >> Dict.fromList
                >> step yOffset Rules.parkour
                >> Dict.insert (1,-yOffset) Stone
                >> Dict.insert (width-2,-yOffset) Stone
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list (width*(height//2)) <| Random.weighted ( 10, Nothing ) [ ( 1, Just Stone ) ] )

floorGenerator : Int -> Generator (Dict Location MapElement)
floorGenerator level =
    let
        yOffset : Int
        yOffset = level*height

        piliar x y =
            [ ((x,-y-yOffset),Stone)
            , ((x+1,-y-yOffset),Stone)
            , ((x+2,-y-yOffset),Stone)
            , ((x,-y+1-yOffset),Stone)
            , ((x+2,-y+1-yOffset),Stone)
            ]

        build : List (Maybe Block) -> List ( Location, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x, -2-yOffset ), elem )))
                >> List.filterMap identity
    in
    Random.map2
        (\seed ->
            build
                >> (\list ->
                        List.concat
                            [ list
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, -1-yOffset ), elem ))
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, 0-yOffset ), elem ))
                            , piliar (width-1-5-2) (height-1)
                            , piliar 5 (height-1)
                            ]
                   )
                >> Dict.fromList
                >> repeat 2 (step yOffset Rules.placeDirt)
                >> repeat 2 (step yOffset Rules.placeGrass)
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list width (Random.weighted ( 3, Nothing ) [ ( 10, Just Dirt ) ]))
