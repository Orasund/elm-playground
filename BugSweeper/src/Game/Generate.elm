module Game.Generate exposing (..)

import BugSpecies exposing (BugSpecies(..))
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Random exposing (Generator)
import Set.Any exposing (AnySet)
import Tile exposing (Tile(..))


type alias Random a =
    Generator a


type Block
    = TileBlock Tile
    | BugBlock BugSpecies
    | EmptyBlock


new : Int -> AnySet String BugSpecies -> Random Game
new level collectedBugs =
    let
        initGame =
            { grid = Dict.empty
            , bugs = Dict.empty
            , collectedBugs = collectedBugs
            , turn = 0
            , level = level
            }
    in
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
                                TileBlock tile ->
                                    Game.placeTile pos tile

                                BugBlock bug ->
                                    Game.placeBug pos bug

                                EmptyBlock ->
                                    identity
                        )
                        initGame
            )


place : Block -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
place block =
    case block of
        TileBlock tile ->
            placeTile tile

        BugBlock Ant ->
            placeAnt

        BugBlock Caterpillar ->
            placeCaterpillar

        BugBlock Worm ->
            placeWorm

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


placeTile : Tile -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeTile tile game =
    case emptyPositions game of
        head :: tail ->
            Random.uniform head tail
                |> Random.map (\pos -> Dict.insert pos (TileBlock tile) game)

        [] ->
            Random.constant game


placeAnt : Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeAnt dict =
    dict
        |> emptyPositions
        |> List.filter
            (\( x, y ) ->
                neighborsOf ( x, y )
                    |> List.all
                        (\pos ->
                            Dict.get pos dict
                                |> Maybe.map ((==) EmptyBlock)
                                |> Maybe.withDefault True
                        )
            )
        |> (\list ->
                case list of
                    head :: tail ->
                        Random.uniform head tail
                            |> Random.map
                                (\pos ->
                                    neighborsOf pos
                                        |> List.foldl (\p -> Dict.insert p EmptyBlock)
                                            dict
                                        |> Dict.insert pos (BugBlock Ant)
                                )

                    [] ->
                        Random.constant dict
           )


placeCaterpillar : Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeCaterpillar dict =
    dict
        |> emptyPositions
        |> List.filter
            (\( x, y ) ->
                neighborsOf ( x, y )
                    |> List.any
                        (\pos ->
                            Dict.get pos dict
                                |> Maybe.map ((==) (TileBlock Leaf))
                                |> Maybe.withDefault True
                        )
            )
        |> pick
        |> Maybe.map
            (Random.andThen
                (\pos ->
                    ensureAtLeastOneOf (TileBlock Leaf) pos dict
                        |> Random.map (Dict.insert pos (BugBlock Caterpillar))
                )
            )
        |> Maybe.withDefault (Random.constant dict)


placeWorm : Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeWorm dict =
    dict
        |> emptyPositions
        |> List.filter
            (\( x, y ) ->
                neighborsOf ( x, y )
                    |> List.any
                        (\pos ->
                            Dict.get pos dict
                                |> Maybe.map ((==) (TileBlock Stone))
                                |> Maybe.withDefault True
                        )
            )
        |> pick
        |> Maybe.map
            (Random.andThen
                (\pos ->
                    ensureAtLeastOneOf (TileBlock Stone) pos dict
                        |> Random.map (Dict.insert pos (BugBlock Worm))
                )
            )
        |> Maybe.withDefault (Random.constant dict)


placeBug : BugSpecies -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
placeBug bug game =
    case emptyPositions game of
        head :: tail ->
            Random.uniform head tail
                |> Random.map (\pos -> Dict.insert pos (BugBlock bug) game)

        [] ->
            Random.constant game


ensureAtLeastOneOf : Block -> ( Int, Int ) -> Dict ( Int, Int ) Block -> Random (Dict ( Int, Int ) Block)
ensureAtLeastOneOf block pos dict =
    if
        neighborsOf pos
            |> List.any (\p -> Dict.get p dict == Just block)
    then
        Random.constant dict

    else
        neighborsOf pos
            |> List.filter (\p -> Dict.member p dict |> not)
            |> pick
            |> Maybe.map
                (Random.map
                    (\p ->
                        Dict.insert p block dict
                    )
                )
            |> Maybe.withDefault (Random.constant dict)


pick : List a -> Maybe (Random a)
pick list =
    case list of
        head :: tail ->
            Random.uniform head tail |> Just

        [] ->
            Nothing


neighborsOf : ( Int, Int ) -> List ( Int, Int )
neighborsOf ( x, y ) =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]


emptyPositions : Dict ( Int, Int ) Block -> List ( Int, Int )
emptyPositions dict =
    List.range 0 (Config.gridSize - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Config.gridSize - 1)
                    |> List.map (Tuple.pair x)
            )
        |> List.filter (\pos -> Dict.get pos dict == Nothing)
