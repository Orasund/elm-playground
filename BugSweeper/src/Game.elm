module Game exposing (Bug, Game, isValidPos, placeBug, placeTile, reveal)

import BugSpecies exposing (BugSpecies(..))
import Config
import Dict exposing (Dict)
import Set exposing (Set)
import Set.Any as AnySet exposing (AnySet)
import Tile exposing (Tile(..))


type alias Bug =
    { visible : Bool
    , species : BugSpecies
    }


type alias Game =
    { grid : Dict ( Int, Int ) Tile
    , bugs : Dict ( Int, Int ) BugSpecies
    , collectedBugs : AnySet String BugSpecies
    , turn : Int
    , level : Int
    , revealed : Set ( Int, Int )
    }


placeTile : ( Int, Int ) -> Tile -> Game -> Game
placeTile pos tile game =
    { game | grid = Dict.insert pos tile game.grid }


placeBug : ( Int, Int ) -> BugSpecies -> Game -> Game
placeBug pos bug game =
    { game | bugs = Dict.insert pos bug game.bugs }


reveal : ( Int, Int ) -> Game -> Game
reveal pos game =
    case game.bugs |> Dict.get pos of
        Just bug ->
            { game
                | revealed = Set.insert pos game.revealed
                , collectedBugs = game.collectedBugs |> AnySet.insert bug
            }

        _ ->
            { game
                | revealed = Set.insert pos game.revealed
                , turn =
                    if Dict.get pos game.grid == Nothing then
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
