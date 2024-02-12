module Game exposing (Game, Tile(..), empty, isOver, isValidPos, placeBug, placeObject, reveal)

import BugSpecies exposing (BugSpecies(..))
import Collection exposing (Collection, Variant(..))
import Config
import Dict exposing (Dict)
import Object exposing (Object(..))


type Tile
    = BugTile BugSpecies
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
    game.remainingGuesses
        <= 0
        || (game.tiles
                |> Dict.filter
                    (\pos tile ->
                        case tile of
                            BugTile _ ->
                                Dict.member pos game.revealed |> not

                            _ ->
                                False
                    )
                |> Dict.isEmpty
           )


placeObject : ( Int, Int ) -> Object -> Game -> Game
placeObject pos tile game =
    { game | tiles = Dict.insert pos (ObjectTile tile) game.tiles }


placeBug : ( Int, Int ) -> BugSpecies -> Game -> Game
placeBug pos bug game =
    { game | tiles = Dict.insert pos (BugTile bug) game.tiles }


reveal : ( Int, Int ) -> Game -> Game
reveal pos game =
    let
        variant =
            Cute
    in
    { game
        | revealed = Dict.insert pos variant game.revealed
        , collected =
            case Dict.get pos game.tiles of
                Just (BugTile bug) ->
                    game.collected
                        |> Collection.insert bug Cute

                _ ->
                    game.collected
        , remainingGuesses =
            if Dict.get pos game.tiles == Nothing then
                game.remainingGuesses - 1

            else
                game.remainingGuesses
    }


isValidPos : ( Int, Int ) -> Bool
isValidPos ( x, y ) =
    (0 <= x)
        && (x < Config.gridSize)
        && (0 <= y)
        && (y < Config.gridSize)
