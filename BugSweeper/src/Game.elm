module Game exposing (Game, Tile(..), empty, isOver, isValidPos, isWon, neighborsOf, placeBug, placeObject, reveal)

import Bug exposing (Bug(..))
import Collection exposing (Collection, Variant(..))
import Config
import Dict exposing (Dict)
import Object exposing (Object(..))


type Tile
    = BugTile Bug
    | ObjectTile Object


type alias Game =
    { tiles : Dict ( Int, Int ) Tile
    , remainingGuesses : Int
    , level : Int
    , revealed : Dict ( Int, Int ) Variant
    , collected : Collection
    }


empty : Int -> Game
empty level =
    { tiles = Dict.empty
    , remainingGuesses = Config.startingGuesses
    , level = level
    , revealed = Dict.empty
    , collected = Dict.empty
    }


isOver : Game -> Bool
isOver game =
    isLost game
        || isWon game


isLost : Game -> Bool
isLost game =
    game.remainingGuesses
        <= 0


isWon : Game -> Bool
isWon game =
    game.tiles
        |> Dict.filter
            (\pos tile ->
                case tile of
                    BugTile _ ->
                        Dict.member pos game.revealed |> not

                    _ ->
                        False
            )
        |> Dict.isEmpty


placeObject : ( Int, Int ) -> Object -> Game -> Game
placeObject pos tile game =
    { game | tiles = Dict.insert pos (ObjectTile tile) game.tiles }


placeBug : ( Int, Int ) -> Bug -> Game -> Game
placeBug pos bug game =
    { game | tiles = Dict.insert pos (BugTile bug) game.tiles }


reveal : ( Int, Int ) -> Variant -> Game -> Game
reveal pos variant game =
    { game
        | revealed = Dict.insert pos variant game.revealed
        , collected =
            case Dict.get pos game.tiles of
                Just (BugTile bug) ->
                    game.collected
                        |> Collection.insert 1 bug variant

                _ ->
                    game.collected
        , remainingGuesses =
            if Dict.get pos game.tiles == Nothing then
                game.remainingGuesses - 1

            else
                game.remainingGuesses
    }


neighborsOf : ( Int, Int ) -> List ( Int, Int )
neighborsOf ( x, y ) =
    [ ( x + 1, y )
    , ( x - 1, y )
    , ( x, y + 1 )
    , ( x, y - 1 )
    ]
        |> List.filter isValidPos


isValidPos : ( Int, Int ) -> Bool
isValidPos ( x, y ) =
    (0 <= x)
        && (x < Config.gridSize)
        && (0 <= y)
        && (y < Config.gridSize)
