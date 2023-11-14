module Game.Solve exposing (..)

import Dict exposing (Dict)
import Game exposing (Game)
import Set exposing (Set)


type alias Node =
    List ( ( Int, Int ), Int )


type alias Graph =
    { nodes : Set Node
    , winning : Set Node
    }


validMoves : Game -> List Game
validMoves game =
    if Game.gameWon game then
        []

    else
        game.tiles
            |> List.filterMap
                (\id ->
                    Game.move id game
                )


buildGraph : Game -> Graph
buildGraph game =
    let
        getNodes :
            { remaining : List Node
            , nodes : Set Node
            , winning : Set Node
            , pathTo : Dict Node (Set Node)
            }
            ->
                { nodes : Set Node
                , winning : Set Node
                , pathTo : Dict Node (Set Node)
                }
        getNodes args =
            case args.remaining of
                head :: tail ->
                    let
                        g : Game
                        g =
                            { game | board = Dict.fromList head }

                        moves : List Node
                        moves =
                            validMoves g
                                |> List.map (\{ board } -> Dict.toList board)
                    in
                    if Set.member head args.nodes then
                        { args | remaining = tail } |> getNodes

                    else
                        { remaining = moves ++ tail
                        , nodes = args.nodes |> Set.union (Set.fromList moves)
                        , winning =
                            if moves == [] && Game.gameWon g then
                                args.winning |> Set.insert head

                            else
                                args.winning
                        , pathTo =
                            moves
                                |> List.foldl
                                    (\m ->
                                        Dict.update m
                                            (\maybe ->
                                                maybe
                                                    |> Maybe.map (Set.insert head)
                                                    |> Maybe.withDefault (Set.singleton head)
                                                    |> Just
                                            )
                                    )
                                    args.pathTo
                        }
                            |> getNodes

                [] ->
                    { nodes = args.nodes
                    , winning = args.winning
                    , pathTo = args.pathTo
                    }

        getWinnings :
            { nodes : Set Node
            , remaining : Set Node
            , output : Set Node
            , pathTo : Dict Node (Set Node)
            }
            -> Set Node
        getWinnings args =
            if Set.isEmpty args.remaining then
                args.output

            else
                args.remaining
                    |> Set.foldl
                        (\a ->
                            Set.union
                                (args.pathTo
                                    |> Dict.get a
                                    |> Maybe.withDefault Set.empty
                                )
                        )
                        Set.empty
                    |> (\set ->
                            { args
                                | remaining = set
                                , output = Set.union args.output set
                            }
                                |> getWinnings
                       )
    in
    getNodes
        { remaining = List.singleton (Dict.toList game.board)
        , nodes = Set.empty
        , winning = Set.empty
        , pathTo = Dict.empty
        }
        |> (\{ winning, nodes, pathTo } ->
                { nodes = nodes
                , winning =
                    getWinnings
                        { nodes = nodes
                        , remaining = winning
                        , output = winning
                        , pathTo = pathTo
                        }
                }
           )


findSoftlocks : Game -> Set Node
findSoftlocks game =
    let
        graph =
            game
                |> buildGraph
    in
    graph.nodes |> Set.diff graph.winning
