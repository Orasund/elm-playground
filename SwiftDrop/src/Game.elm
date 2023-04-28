module Game exposing (..)

import Config
import Dict exposing (Dict)
import Game.Type exposing (Game)
import Random exposing (Generator)


addTile : ( Int, Int ) -> Int -> Game -> Game
addTile pos value game =
    { game
        | tiles = game.tiles |> Dict.insert game.nextTileId value
        , grid = game.grid |> Dict.insert pos game.nextTileId
        , nextTileId = game.nextTileId + 1
    }


generate : Random.Generator Game
generate =
    Random.int 1 5
        |> Random.list (Config.size * Config.size)
        |> Random.map
            (\list ->
                list
                    |> List.indexedMap
                        (\i value ->
                            ( ( i |> modBy Config.size, i // Config.size )
                            , value
                            )
                        )
                    |> List.foldl (\( pos, value ) -> addTile pos value)
                        Game.Type.empty
            )


dragRow : { row : Int, amount : Int } -> Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
dragRow args readDict =
    List.range 0 (Config.size - 1)
        |> List.foldl
            (\x writeDict ->
                readDict
                    |> Dict.get ( x, args.row )
                    |> Maybe.map
                        (\tile ->
                            writeDict
                                |> Dict.insert
                                    ( x + args.amount |> modBy Config.size
                                    , args.row
                                    )
                                    tile
                        )
                    |> Maybe.withDefault writeDict
            )
            readDict


dragColumn : { column : Int, amount : Int } -> Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
dragColumn args readDict =
    List.range 0 (Config.size - 1)
        |> List.foldl
            (\y writeDict ->
                readDict
                    |> Dict.get ( args.column, y )
                    |> Maybe.map
                        (\tile ->
                            writeDict
                                |> Dict.insert
                                    ( args.column
                                    , y + args.amount |> modBy Config.size
                                    )
                                    tile
                        )
                    |> Maybe.withDefault writeDict
            )
            readDict


drag : { from : ( Int, Int ), to : ( Int, Int ) } -> Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
drag args grid =
    let
        ( fromX, fromY ) =
            args.from

        ( toX, toY ) =
            args.to
    in
    if fromX == toX then
        grid
            |> dragColumn
                { column = fromX
                , amount = toY - fromY |> modBy Config.size
                }

    else if fromY == toY then
        grid
            |> dragRow
                { row = fromY
                , amount = toX - fromX |> modBy Config.size
                }

    else
        grid


refill : Game -> Generator Game
refill game =
    game
        |> getEmptyPositions
        |> List.foldl
            (\pos ->
                Random.andThen
                    (\g ->
                        Random.int 1 5
                            |> Random.map
                                (\value ->
                                    addTile pos value g
                                )
                    )
            )
            (Random.constant game)


getEmptyPositions : Game -> List ( Int, Int )
getEmptyPositions game =
    List.range 0 (Config.size - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (Config.size - 1)
                    |> List.map
                        (\x ->
                            ( x, y )
                        )
            )
        |> List.filter
            (\pos ->
                game.grid
                    |> Dict.get pos
                    |> (==) Nothing
            )
