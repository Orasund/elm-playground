module Game.Type exposing (..)

import Dict exposing (Dict)


type alias TileId =
    Int


type alias Game =
    { tiles : Dict TileId Int
    , nextTileId : TileId
    , grid : Dict ( Int, Int ) TileId
    }


empty : Game
empty =
    { tiles = Dict.empty
    , nextTileId = 0
    , grid = Dict.empty
    }


updateValueAtPosition : ( Int, Int ) -> (Int -> Int) -> Game -> Game
updateValueAtPosition pos fun game =
    game
        |> getTileByPosition pos
        |> Maybe.map
            (\( tileId, value ) ->
                game.tiles
                    |> Dict.insert tileId (fun value)
                    |> (\tiles -> { game | tiles = tiles })
            )
        |> Maybe.withDefault game


removeTileAtPosition : ( Int, Int ) -> Game -> Game
removeTileAtPosition pos game =
    game.grid
        |> Dict.get pos
        |> Maybe.map
            (\tileId ->
                { game
                    | tiles = game.tiles |> Dict.remove tileId
                    , grid = game.grid |> Dict.remove pos
                }
            )
        |> Maybe.withDefault game


getTileByPosition : ( Int, Int ) -> Game -> Maybe ( TileId, Int )
getTileByPosition pos game =
    game.grid
        |> Dict.get pos
        |> Maybe.andThen
            (\tileId ->
                game.tiles
                    |> Dict.get tileId
                    |> Maybe.map (Tuple.pair tileId)
            )
