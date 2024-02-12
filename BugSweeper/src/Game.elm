module Game exposing (Game, Tile(..), isValidPos, placeBug, placeObject, reveal)

import BugSpecies exposing (BugSpecies(..))
import Config
import Dict exposing (Dict)
import Object exposing (Object(..))
import Set exposing (Set)
import Set.Any as AnySet exposing (AnySet)


type Tile
    = BugTile BugSpecies
    | ObjectTile Object


type alias Game =
    { tiles : Dict ( Int, Int ) Tile
    , collectedBugs : AnySet String BugSpecies
    , turn : Int
    , level : Int
    , revealed : Set ( Int, Int )
    }


placeObject : ( Int, Int ) -> Object -> Game -> Game
placeObject pos tile game =
    { game | tiles = Dict.insert pos (ObjectTile tile) game.tiles }


placeBug : ( Int, Int ) -> BugSpecies -> Game -> Game
placeBug pos bug game =
    { game | tiles = Dict.insert pos (BugTile bug) game.tiles }


reveal : ( Int, Int ) -> Game -> Game
reveal pos game =
    { game
        | revealed = Set.insert pos game.revealed
        , collectedBugs =
            case game.tiles |> Dict.get pos of
                Just (BugTile bug) ->
                    game.collectedBugs |> AnySet.insert bug

                _ ->
                    game.collectedBugs
        , turn =
            if Dict.get pos game.tiles == Nothing then
                game.turn + 1

            else
                game.turn
    }


isValidPos : ( Int, Int ) -> Bool
isValidPos ( x, y ) =
    (0 <= x)
        && (x < Config.gridSize)
        && (0 <= y)
        && (y < Config.gridSize)
