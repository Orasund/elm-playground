module Game exposing (Game, moveBugs, nearestBug, new, positions, removeCatchedBugs, removeLeafs, reveal)

import BugSpecies exposing (BugSpecies(..))
import Config
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List
import Set.Any as AnySet exposing (AnySet)
import Tile exposing (Tile(..))


type alias Bug =
    { visible : Bool
    , species : BugSpecies
    }


type alias Game =
    { grid : Dict ( Int, Int ) Tile
    , bugs : Dict ( Int, Int ) Bug
    , collectedBugs : AnySet String BugSpecies
    , turn : Int
    , level : Int
    }


new : Int -> AnySet String BugSpecies -> Generator Game
new level collectedBugs =
    positions
        |> Random.List.choices Config.leafAmount
        |> Random.andThen
            (\( leafs, rest ) ->
                rest
                    |> Random.List.choices Config.stoneAmount
                    |> Random.andThen
                        (\( stones, rest2 ) ->
                            Random.map2
                                (\( bugs, _ ) list ->
                                    { grid =
                                        List.map (\pos -> ( pos, Leaf )) leafs
                                            ++ List.map (\pos -> ( pos, Stone )) stones
                                            |> Dict.fromList
                                    , bugs =
                                        List.map2
                                            (\pos species ->
                                                ( pos
                                                , { visible = False
                                                  , species = species
                                                  }
                                                )
                                            )
                                            bugs
                                            list
                                            |> Dict.fromList
                                    , collectedBugs = collectedBugs
                                    , turn = 0
                                    , level = level
                                    }
                                )
                                (Random.List.choices Config.bugAmount rest2)
                                (Random.list Config.bugAmount (BugSpecies.generate level))
                        )
            )


reveal : ( Int, Int ) -> Game -> Game
reveal pos game =
    case game.bugs |> Dict.get pos of
        Just bug ->
            { game
                | bugs = game.bugs |> Dict.insert pos { bug | visible = True }
                , collectedBugs = game.collectedBugs |> AnySet.insert bug.species
                , turn = game.turn - 1
            }

        _ ->
            case game.grid |> Dict.get pos of
                Just Leaf ->
                    { game
                        | grid = game.grid |> Dict.remove pos
                    }

                Just _ ->
                    game

                Nothing ->
                    { game
                        | grid = game.grid |> Dict.insert pos Leaf
                    }


positions : List ( Int, Int )
positions =
    List.range 0 (Config.gridSize - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Config.gridSize - 1)
                    |> List.map (Tuple.pair x)
            )


internalBugMovement : ( Int, Int ) -> Bug -> Game -> Generator Game
internalBugMovement ( x, y ) bug g =
    (case bug.species of
        Grasshopper ->
            [ ( x + 1, y + 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y + 1 )
            , ( x - 1, y - 1 )
            , ( x, y - 2 )
            , ( x - 2, y )
            , ( x, y + 2 )
            , ( x + 2, y )
            ]

        LadyBeetle ->
            [ ( x, y - 1 )
            , ( x - 1, y )
            , ( x, y + 1 )
            , ( x + 1, y )
            , ( x, y - 2 )
            , ( x - 2, y )
            , ( x, y + 2 )
            , ( x + 2, y )
            ]

        _ ->
            [ ( x, y - 1 )
            , ( x - 1, y )
            , ( x, y + 1 )
            , ( x + 1, y )
            ]
    )
        |> List.filter
            (\( x0, y0 ) ->
                (0 <= x0)
                    && (x0 < Config.gridSize)
                    && (0 <= y0)
                    && (y0 < Config.gridSize)
            )
        |> List.filterMap
            (\p ->
                case ( g.grid |> Dict.get p, g.bugs |> Dict.get p ) of
                    ( Nothing, Nothing ) ->
                        Just ( p, Nothing )

                    ( Just Stone, Nothing ) ->
                        Just ( p, Just Stone )

                    ( Just SpiderWeb, Nothing ) ->
                        Just ( p, Just SpiderWeb )

                    ( _, _ ) ->
                        Nothing
            )
        |> (case bug.species of
                Cockroach ->
                    \list ->
                        let
                            ( stone, empty ) =
                                List.partition
                                    (\( _, maybe ) ->
                                        maybe == Just Stone
                                    )
                                    list
                        in
                        if List.isEmpty stone then
                            Random.constant empty

                        else
                            case empty of
                                h :: t ->
                                    Random.uniform h t
                                        |> Random.map (\e -> e :: stone)

                                [] ->
                                    Random.constant stone

                Grasshopper ->
                    \list ->
                        let
                            ( stone, empty ) =
                                List.partition
                                    (\( _, maybe ) ->
                                        maybe == Just Stone
                                    )
                                    list
                        in
                        (if List.isEmpty empty then
                            stone

                         else
                            empty
                        )
                            |> Random.constant

                Snail ->
                    \list ->
                        let
                            ( _, empty ) =
                                List.partition
                                    (\( _, maybe ) ->
                                        maybe == Just Stone
                                    )
                                    list
                        in
                        (case g.grid |> Dict.get ( x, y ) of
                            Just Stone ->
                                empty

                            _ ->
                                []
                        )
                            |> Random.constant

                _ ->
                    Random.constant
           )
        |> Random.andThen Random.List.choose
        |> Random.andThen
            (\( maybe, _ ) ->
                case bug.species of
                    Spider ->
                        Random.int 0 4
                            |> Random.map
                                (\int ->
                                    if int == 0 then
                                        { g
                                            | grid =
                                                g.grid
                                                    |> Dict.update ( x, y ) (\m -> m |> Maybe.withDefault SpiderWeb |> Just)
                                            , bugs =
                                                maybe
                                                    |> Maybe.map
                                                        (\( p, _ ) ->
                                                            g.bugs
                                                                |> Dict.remove ( x, y )
                                                                |> Dict.insert p { bug | visible = False }
                                                        )
                                                    |> Maybe.withDefault g.bugs
                                        }

                                    else
                                        { g
                                            | bugs =
                                                maybe
                                                    |> Maybe.map
                                                        (\( p, _ ) ->
                                                            g.bugs
                                                                |> Dict.remove ( x, y )
                                                                |> Dict.insert p { bug | visible = False }
                                                        )
                                                    |> Maybe.withDefault g.bugs
                                        }
                                )

                    _ ->
                        maybe
                            |> Maybe.map
                                (\( p, _ ) ->
                                    g.bugs
                                        |> Dict.remove ( x, y )
                                        |> Dict.insert p { bug | visible = False }
                                )
                            |> Maybe.withDefault g.bugs
                            |> Random.constant
                            |> Random.map (\bugs -> { g | bugs = bugs })
            )


moveBugs : Game -> Generator Game
moveBugs game =
    game.bugs
        |> Dict.toList
        |> List.filter (\( _, { visible } ) -> not visible)
        |> Random.List.shuffle
        |> Random.andThen
            (List.foldl
                (\( p, bug ) -> Random.andThen (internalBugMovement p bug))
                (Random.constant game)
            )


removeCatchedBugs : Game -> Game
removeCatchedBugs game =
    game.bugs
        |> Dict.filter (\_ { visible } -> not visible)
        |> (\bugs -> { game | bugs = bugs })


removeLeafs : ( Int, Int ) -> Game -> Game
removeLeafs ignorePos game =
    game.grid
        |> Dict.toList
        |> List.filterMap
            (\( p, tile ) ->
                if tile == Leaf && p /= ignorePos then
                    Just p

                else
                    Nothing
            )
        |> List.foldl
            (\( x, y ) grid ->
                [ ( x, y - 1 )
                , ( x - 1, y )
                , ( x, y + 1 )
                , ( x + 1, y )
                ]
                    |> List.map
                        (\p ->
                            game.bugs |> Dict.get p
                        )
                    |> List.any
                        (\maybe ->
                            case maybe of
                                Just { visible } ->
                                    not visible

                                Nothing ->
                                    False
                        )
                    |> (\nextToBug ->
                            if nextToBug then
                                grid
                                    |> Dict.remove ( x, y )

                            else
                                grid
                       )
            )
            game.grid
        |> (\grid -> { game | grid = grid })


nearestBug : ( Int, Int ) -> Game -> Int
nearestBug ( x0, y0 ) game =
    game.bugs
        |> Dict.toList
        |> List.filterMap
            (\( p, { visible } ) ->
                if visible then
                    Nothing

                else
                    Just p
            )
        |> List.map
            (\( x1, y1 ) ->
                abs (x0 - x1) + abs (y0 - y1)
            )
        |> List.minimum
        |> Maybe.withDefault 0
