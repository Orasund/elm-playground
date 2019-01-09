module RuineJump.MapSegment exposing (append, concat, floorGenerator, intersectionGenerator, parkourGenerator)

import CellAutomata exposing (Location, Rule, RuleExpression(..),Automata)
import Dict exposing (Dict)
import Natural exposing (Natural16(..))
import Random exposing (Generator)
import RuineJump.Automata as Automata exposing (Grid, automata, mirroringAutomata)
import RuineJump.Config as Config
import RuineJump.Map exposing (Map)
import RuineJump.MapElement exposing (Block(..), MapElement(..))
import RuineJump.Rules as Rules


width : Int
width =
    Config.width


height : Int
height =
    Config.sectionHeight


append : Generator Map -> Generator Map -> Generator Map
append =
    Random.map2
        (\map segment ->
            map |> Dict.union segment
        )


concat : List (Generator Map) -> Generator Map
concat list =
    case list of
        [] ->
            Random.constant Dict.empty

        a :: tail ->
            tail
                |> List.foldl
                    append
                    a


newSeed : Int -> Location -> Random.Seed
newSeed seed ( x, y ) =
    Random.initialSeed (seed + x * width + y)


toSegment : Int -> Grid -> Map
toSegment seed =
    Dict.map
        (\pos block ->
            BlockElement block <|
                Tuple.first <|
                    Random.step (Random.int 0 Random.maxInt) <|
                        newSeed seed <|
                            pos
        )

stepWithAutomata : (List (Rule Block) -> Automata Block) -> Int -> List (Rule Block) -> Grid -> Grid
stepWithAutomata customAutomata yOffset rules grid =
    List.range 0 width
        |> List.foldl
            (\x partList ->
                List.range (-1 * height - yOffset) -yOffset
                    |> List.foldl
                        (\y list ->
                            let
                                pos : Location
                                pos =
                                    ( x, y )

                                elem : Maybe Block
                                elem =
                                    grid |> Dict.get pos
                            in
                            case Automata.step (customAutomata rules) grid pos elem of
                                Just a ->
                                    ( pos, a ) :: list

                                Nothing ->
                                    list
                        )
                        partList
            )
            []
        |> Dict.fromList

step : Int -> List (Rule Block) -> Grid -> Grid
step =
    stepWithAutomata automata

mirroringStep : Int -> List (Rule Block) -> Grid -> Grid
mirroringStep =
    stepWithAutomata mirroringAutomata

repeat : Int -> (a -> a) -> a -> a
repeat num fun dict =
    List.range 0 (num - 1)
        |> List.foldl (always <| fun) dict


intersectionGenerator : Int -> Generator Map
intersectionGenerator level =
    let
        yOffset : Int
        yOffset =
            level * height

        segment : Grid
        segment =
            [ List.repeat height Stone
                |> List.indexedMap (\y elem -> ( ( 0, -y - yOffset ), elem ))
            , List.repeat height Stone
                |> List.indexedMap (\y elem -> ( ( width - 1, -y - yOffset ), elem ))
            , List.repeat (height // 4) Stone
                |> List.indexedMap (\y elem -> ( ( 1, -y * 4 - yOffset ), elem ))
            , List.repeat (height // 4) Stone
                |> List.indexedMap (\y elem -> ( ( width - 2, -y * 4 - yOffset ), elem ))
            , List.repeat (width // 4) Stone
                |> List.indexedMap (\x elem -> ( ( 1 + x * 4, -yOffset ), elem ))
            , List.repeat (width // 4) Stone
                |> List.indexedMap (\x elem -> ( ( 2 + x * 4, -yOffset ), elem ))
            , List.repeat (width // 4) Stone
                |> List.indexedMap (\x elem -> ( ( 1 + x * 4, -(height-1) - yOffset ), elem ))
            , List.repeat (width // 4) Stone
                |> List.indexedMap (\x elem -> ( ( 2 + x * 4, -(height-1) - yOffset ), elem ))
            ]
                |> List.concat
                |> Dict.fromList
                
    in
    Random.map
        (\seed ->
            segment |> toSegment seed
        )
    (Random.int Random.minInt Random.maxInt)


parkourGenerator : Int -> Generator Map
parkourGenerator level =
    let
        yOffset : Int
        yOffset =
            level * height

        build : List (Maybe Block) -> List ( Location, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x |> modBy width, -1 - yOffset - 2 * (x // width) ), elem )))
                >> List.filterMap identity
    in
    Random.map2
        (\seed ->
            build
                >> (\list ->
                        List.concat
                            [ list
                            , List.repeat height Stone
                                |> List.indexedMap (\x elem -> ( ( 0, -x - yOffset ), elem ))
                            , List.repeat height Stone
                                |> List.indexedMap (\x elem -> ( ( width - 1, -x - yOffset ), elem ))
                            ]
                   )
                >> Dict.fromList
                >> step yOffset Rules.parkour
                >> Dict.insert ( 1, -yOffset ) Stone
                >> Dict.insert ( width - 2, -yOffset ) Stone
                >> repeat 2 (step yOffset Rules.placeDirt)
                >> repeat 2 (step yOffset Rules.placeGrass)
                >> repeat 3 (mirroringStep yOffset Rules.removeGrass)
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list (width * (height // 2)) <| Random.weighted ( 2+(toFloat level), Nothing ) [ ( 1+(toFloat <|level//4), Just Stone ),(4,Just Dirt) ])


floorGenerator : Int -> Generator Map
floorGenerator level =
    let
        yOffset : Int
        yOffset =
            level * height

        piliar x y =
            [ ( ( x, -y - yOffset ), Stone )
            , ( ( x + 1, -y - yOffset ), Stone )
            , ( ( x + 2, -y - yOffset ), Stone )
            , ( ( x, -y + 1 - yOffset ), Stone )
            , ( ( x + 2, -y + 1 - yOffset ), Stone )
            ]

        build : List (Maybe Block) -> List ( Location, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x, -2 - yOffset ), elem )))
                >> List.filterMap identity
    in
    Random.map2
        (\seed ->
            build
                >> (\list ->
                        List.concat
                            [ list
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, -1 - yOffset ), elem ))
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, 0 - yOffset ), elem ))
                            , piliar (width - 1 - 5 - 2) (height - 1)
                            , piliar 5 (height - 1)
                            ]
                   )
                >> Dict.fromList
                >> repeat 2 (step yOffset Rules.placeDirt)
                >> repeat 2 (step yOffset Rules.placeGrass)
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list width (Random.weighted ( 3, Nothing ) [ ( 10, Just Dirt ) ]))
