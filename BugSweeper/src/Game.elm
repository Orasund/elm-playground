module Game exposing (Game, moveBug, nearestBug, new, positions, removeCatchedBugs, removeLeafs, reveal)

import Config
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List
import Tile exposing (Tile(..))


type alias Game =
    { grid : Dict ( Int, Int ) Tile }


new : Generator Game
new =
    positions
        |> Random.List.choices Config.leafAmount
        |> Random.andThen
            (\( leafs, rest ) ->
                rest
                    |> Random.List.choices Config.stoneAmount
                    |> Random.andThen
                        (\( stones, rest2 ) ->
                            Random.List.choices Config.bugAmount rest2
                                |> Random.map
                                    (\( bugs, _ ) ->
                                        List.map (\pos -> ( pos, Bug { visible = False } )) bugs
                                            ++ List.map (\pos -> ( pos, Leaf )) leafs
                                            ++ List.map (\pos -> ( pos, Stone )) stones
                                            |> Dict.fromList
                                    )
                        )
            )
        |> Random.map (\dict -> { grid = dict })


reveal : ( Int, Int ) -> Game -> Game
reveal pos game =
    (case game.grid |> Dict.get pos of
        Just (Bug _) ->
            game.grid
                |> Dict.insert pos (Bug { visible = True })

        _ ->
            game.grid
                |> Dict.insert pos Leaf
    )
        |> (\grid -> { game | grid = grid })


positions : List ( Int, Int )
positions =
    List.range 0 (Config.gridSize - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Config.gridSize - 1)
                    |> List.map (Tuple.pair x)
            )


moveBug : Game -> Generator Game
moveBug game =
    game.grid
        |> Dict.toList
        |> List.filterMap
            (\( p, tile ) ->
                if tile == Bug { visible = False } then
                    Just p

                else
                    Nothing
            )
        |> Random.List.shuffle
        |> Random.andThen
            (List.foldl
                (\( x, y ) ->
                    Random.andThen
                        (\grid ->
                            [ ( x, y - 1 )
                            , ( x - 1, y )
                            , ( x, y + 1 )
                            , ( x + 1, y )
                            ]
                                |> List.filter
                                    (\( x0, y0 ) ->
                                        (0 <= x0) && (x0 < Config.gridSize) && (0 <= y0) && (y0 < Config.gridSize)
                                    )
                                |> List.filterMap
                                    (\p ->
                                        case grid |> Dict.get p of
                                            Just _ ->
                                                Nothing

                                            Nothing ->
                                                Just p
                                    )
                                |> Random.List.choose
                                |> Random.map
                                    (\( maybe, _ ) ->
                                        maybe
                                            |> Maybe.map
                                                (\p ->
                                                    grid
                                                        |> Dict.remove ( x, y )
                                                        |> Dict.insert p (Bug { visible = False })
                                                )
                                            |> Maybe.withDefault grid
                                    )
                        )
                )
                (Random.constant game.grid)
            )
        |> Random.map (\grid -> { grid = grid })


removeCatchedBugs : Game -> Game
removeCatchedBugs game =
    game.grid
        |> Dict.filter
            (\_ tile -> tile /= Bug { visible = True })
        |> (\grid -> { grid = grid })


removeLeafs : Game -> Game
removeLeafs game =
    game.grid
        |> Dict.toList
        |> List.filterMap
            (\( p, tile ) ->
                if tile == Leaf then
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
                            grid |> Dict.get p
                        )
                    |> List.any
                        (\p ->
                            p == Just (Bug { visible = False })
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
        |> (\grid -> { grid = grid })


nearestBug : ( Int, Int ) -> Game -> Int
nearestBug ( x0, y0 ) game =
    game.grid
        |> Dict.toList
        |> List.filterMap
            (\( p, tile ) ->
                case tile of
                    Bug { visible } ->
                        if visible then
                            Nothing

                        else
                            Just p

                    _ ->
                        Nothing
            )
        |> List.map
            (\( x1, y1 ) ->
                abs (x0 - x1) + abs (y0 - y1)
            )
        |> List.minimum
        |> Maybe.withDefault 0
