module DungeonSokoban.Data.Game exposing (..)

import Css exposing (true)
import Dict exposing (Dict)
import Direction as Direction exposing (Direction(..))
import DungeonSokoban.Config as Config
import DungeonSokoban.Data.Cell as Cell exposing (Cell(..))
import DungeonSokoban.Data.Game.Internal as Internal
import DungeonSokoban.Data.Game.Level1 as Level1
import Grid exposing (Grid)
import PixelEngine exposing (game)
import Position
import Random exposing (Generator)
import Random.List


type alias Game =
    Internal.Game


new : Int -> Generator Game
new level =
    Level1.generate
        |> Random.map fromBoard


fromBoard : Grid (Maybe Cell) -> Game
fromBoard board =
    { board =
        board
            |> Grid.remove Config.playerInit
    , player = Config.playerInit
    }


update : Direction -> Game -> Generator Game
update dir game =
    game.board
        |> Grid.toList
        |> List.filterMap
            (\( pos, maybeCell ) ->
                maybeCell |> Maybe.map (\cell -> ( pos, cell ))
            )
        |> Random.List.shuffle
        |> Random.map (List.foldl (Internal.updateCell dir) game)


movePlayer : Direction -> Game -> Game
movePlayer dir game =
    let
        player =
            dir
                |> Direction.toCoord
                |> Position.addTo game.player
    in
    game.board
        |> Grid.get player
        |> Maybe.map
            (\cell ->
                case cell of
                    Nothing ->
                        { game | player = player }

                    Just Box ->
                        { game | player = player }
                            |> push dir player
                            |> Maybe.withDefault game

                    Just (Monster stunned) ->
                        { game | player = player }
                            |> push dir player
                            {--|> Maybe.map
                                (\g ->
                                    g
                                        |> push dir
                                            (dir
                                                |> Direction.toCoord
                                                |> Position.addTo player
                                            )
                                        |> Maybe.withDefault g
                                )--}
                            |> Maybe.withDefault game

                    Just Hole ->
                        game
            )
        |> Maybe.withDefault game


push : Direction -> ( Int, Int ) -> Game -> Maybe Game
push dir oldPos game =
    let
        pos =
            dir
                |> Direction.toCoord
                |> Position.addTo oldPos
    in
    game.board
        |> Grid.getMember oldPos
        |> Maybe.andThen
            (\cell ->
                Cell.pushable cell
                    |> Maybe.andThen
                        (\pushedCell ->
                            game.board
                                |> Grid.get pos
                                |> Maybe.map
                                    (\newCell ->
                                        case newCell of
                                            Nothing ->
                                                Just
                                                    { game
                                                        | board =
                                                            game.board
                                                                |> Grid.remove oldPos
                                                                |> Grid.insert pos pushedCell
                                                    }

                                            Just Box ->
                                                Nothing

                                            Just (Monster _) ->
                                                Nothing

                                            Just Hole ->
                                                Just
                                                    { game
                                                        | board =
                                                            game.board
                                                                |> Grid.remove oldPos
                                                    }
                                    )
                                |> Maybe.withDefault Nothing
                        )
            )
