module GreenFields.Data.Game exposing (Game, empty, getNeighbors, getTile, hasNoActives, restore)

import Dict exposing (Dict)
import GreenFields.Data.Tile as Tile exposing (Tile)
import Set exposing (Set)
import Time exposing (Posix)


type Game
    = Game
        { board : Dict ( Int, Int ) Tile
        , activeTiles : Set ( Int, Int )
        }


empty : Game
empty =
    Game { board = Dict.empty, activeTiles = Set.empty }


getNeighbors : ( Int, Int ) -> Game -> List Tile
getNeighbors ( x, y ) (Game game) =
    [ ( 1, 0 )
    , ( 0, 1 )
    , ( -1, 0 )
    , ( 0, -1 )
    ]
        |> List.filterMap
            (Tuple.mapBoth ((+) x) ((+) y)
                >> (\pos -> game.board |> Dict.get pos)
            )


hasNoActives : Game -> Bool
hasNoActives (Game game) =
    game.activeTiles |> Set.isEmpty


isActive : ( Int, Int ) -> Game -> Bool
isActive pos (Game game) =
    game.activeTiles |> Set.member pos


getTile : ( Int, Int ) -> Game -> Maybe Tile
getTile ( x, y ) (Game game) =
    case game.board |> Dict.get ( x, y ) of
        Just tile ->
            Just tile

        Nothing ->
            if (x |> modBy 32 |> (==) 0) && (y |> modBy 32 |> (==) 0) then
                Just <| Tile.generate ( x, y )

            else
                Nothing


restore : { position : ( Int, Int ), timestamp : Posix } -> Game -> Game
restore arg (Game game) =
    Game
        { game
            | activeTiles = game.activeTiles |> Set.insert arg.position
            , board =
                game.board
                    |> Dict.update arg.position
                        (Maybe.withDefault (Tile.generate arg.position)
                            >> (\tile -> Debug.log "updated" <| { tile | timestamp = arg.timestamp })
                            >> Just
                        )
        }
