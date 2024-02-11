module Game exposing (Bug, Game, nearestBug, new, positions, removeCatchedBugs, removeLeafs, reveal)

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
