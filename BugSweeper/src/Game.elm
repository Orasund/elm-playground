module Game exposing (Game, Tile(..), isOver, isValidPos, placeBug, placeObject, reveal)

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
    , remainingGuesses : Int
    , level : Int
    , revealed : Set ( Int, Int )
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
                                Set.member pos game.revealed |> not

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
    { game
        | revealed = Set.insert pos game.revealed
        , collectedBugs =
            case game.tiles |> Dict.get pos of
                Just (BugTile bug) ->
                    game.collectedBugs |> AnySet.insert bug

                _ ->
                    game.collectedBugs
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
