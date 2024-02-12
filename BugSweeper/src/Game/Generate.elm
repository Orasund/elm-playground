module Game.Generate exposing (new)

import BugSpecies exposing (BugSpecies(..))
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Object exposing (Object(..))
import Random exposing (Generator)
import Random.List


type alias Random a =
    Generator a


type Block
    = ObjectBlock Object
    | BugBlock BugSpecies
    | EmptyBlock


new : Int -> Random Game
new level =
    Random.list Config.bugAmount (BugSpecies.generate level |> Random.map BugBlock)
        --  |> Random.map ((++) (List.repeat Config.leafAmount (TileBlock Leaf)))
        --|> Random.map ((++) (List.repeat Config.stoneAmount (TileBlock Stone)))
        |> Random.andThen
            (\list ->
                list
                    |> List.foldl (\tile -> Random.andThen (place tile))
                        (Random.constant Dict.empty)
            )
        |> Random.map
            (\dict ->
                dict
                    |> Dict.foldl
                        (\pos block ->
                            case block of
                                ObjectBlock tile ->
                                    Game.placeObject pos tile

                                BugBlock bug ->
                                    Game.placeBug pos bug

                                EmptyBlock ->
                                    identity
                        )
                        (Game.empty level)
            )


place : Block -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
place block =
    case block of
        ObjectBlock tile ->
            placeTile tile

        BugBlock bug ->
            placeBug bug

        EmptyBlock ->
            placeEmpty


placeEmpty : Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeEmpty dict =
    case emptyPositions dict of
        head :: tail ->
            Random.uniform head tail
                |> Random.map (\pos -> Dict.insert pos EmptyBlock dict)

        [] ->
            Random.constant dict


placeTile : Object -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeTile tile game =
    case emptyPositions game of
        head :: tail ->
            Random.uniform head tail
                |> Random.map (\pos -> Dict.insert pos (ObjectBlock tile) game)

        [] ->
            Random.constant game


placeBug : BugSpecies -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeBug bug dict =
    let
        requirements =
            BugSpecies.requirementsOf bug
    in
    dict
        |> emptyPositions
        |> List.filter
            (\pos ->
                let
                    neighbors =
                        Game.neighborsOf pos
                            |> List.map
                                (\p ->
                                    Dict.get p dict
                                        |> Tuple.pair p
                                )

                    requirementsAreMet =
                        requirements
                            |> List.all
                                (\( n, block ) ->
                                    n
                                        <= (neighbors
                                                |> List.filter
                                                    (\( _, maybe ) ->
                                                        (maybe == Nothing)
                                                            || (block
                                                                    |> Maybe.map ObjectBlock
                                                                    |> Maybe.withDefault EmptyBlock
                                                                    |> Just
                                                                    |> (==) maybe
                                                               )
                                                    )
                                                |> List.length
                                           )
                                )

                    enoughSpace =
                        (requirements |> List.map Tuple.first |> List.sum)
                            <= (neighbors
                                    |> List.filter
                                        (\( _, maybe ) ->
                                            maybe
                                                |> Maybe.map
                                                    (\block ->
                                                        requirements
                                                            |> List.any
                                                                (\( _, maybeTile ) ->
                                                                    maybeTile
                                                                        |> Maybe.map ObjectBlock
                                                                        |> Maybe.withDefault EmptyBlock
                                                                        |> (==) block
                                                                )
                                                    )
                                                |> Maybe.withDefault True
                                        )
                                    |> List.length
                               )
                in
                requirementsAreMet && enoughSpace
            )
        |> pick
        |> Maybe.map
            (Random.andThen
                (\pos ->
                    requirements
                        |> List.foldl
                            (\( n, block ) ->
                                Random.andThen
                                    (ensureAtLeast n
                                        (block
                                            |> Maybe.map ObjectBlock
                                            |> Maybe.withDefault EmptyBlock
                                        )
                                        pos
                                    )
                            )
                            (Random.constant dict)
                        |> Random.map (Dict.insert pos (BugBlock bug))
                )
            )
        |> Maybe.withDefault (Random.constant dict)


ensureAtLeast : Int -> Block -> ( Int, Int ) -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
ensureAtLeast n block pos dict =
    let
        remaining =
            n
                - (Game.neighborsOf pos
                    |> List.filter (\p -> Dict.get p dict == Just block)
                    |> List.length
                  )
    in
    Game.neighborsOf pos
        |> List.filter (\p -> Dict.member p dict |> not)
        |> Random.List.choices remaining
        |> Random.map
            (\( l, _ ) ->
                l |> List.foldl (\p -> Dict.insert p block) dict
            )


pick : List a -> Maybe (Random a)
pick list =
    case list of
        head :: tail ->
            Random.uniform head tail |> Just

        [] ->
            Nothing


emptyPositions : Dict ( Int, Int ) Block -> List ( Int, Int )
emptyPositions dict =
    List.range 0 (Config.gridSize - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Config.gridSize - 1)
                    |> List.map (Tuple.pair x)
            )
        |> List.filter (\pos -> Dict.get pos dict == Nothing)
