module Data.Game.Train exposing (..)

import Config
import Data.Block
import Data.Game exposing (Game)
import Data.Game.Wagon
import Data.Game.Wall
import Data.Position
import Data.Train
import Dict
import Random exposing (Generator)


passTime : Game -> Generator Game
passTime game =
    let
        newPos =
            game.train |> Data.Train.forwardPos
    in
    if game.train.pos == Config.hqPos then
        { game
            | train =
                game.train
                    |> Data.Train.addTracks 8
                    |> Data.Train.turnAround
        }
            |> move
            |> Random.constant

    else if game.train.moving && (game.player.pos /= newPos) then
        Dict.get newPos game.world
            |> Maybe.map
                (\block ->
                    case block of
                        Data.Block.Track ->
                            move game |> Random.constant

                        Data.Block.Ground maybeItem ->
                            if game.train.tracks > 0 then
                                maybeItem
                                    |> Maybe.map (\item -> game.train |> Data.Train.addItem item)
                                    |> Maybe.withDefault game.train
                                    |> (\train -> { game | train = train })
                                    |> placeTrack
                                    |> mine

                            else
                                turnToHQ game
                                    |> Random.constant

                        Data.Block.Wagon list ->
                            newPos
                                |> (\( x, y ) -> Random.uniform ( x - 1, y ) [ ( x + 1, y ) ])
                                |> Random.map
                                    (\newWagonPos ->
                                        game
                                            |> Data.Game.Wagon.moveTo newWagonPos ( newPos, list )
                                    )

                        _ ->
                            game
                                |> mine
                                |> Random.map turnToHQ
                )
            |> Maybe.withDefault (Random.constant game)

    else
        Random.constant game


turnToHQ : Game -> Game
turnToHQ game =
    { game | train = game.train |> Data.Train.stop |> Data.Train.turnAround }


placeTrack : Game -> Game
placeTrack game =
    let
        newPos =
            Data.Train.forwardPos game.train
    in
    game.train
        |> Data.Train.removeTrack
        |> Maybe.map
            (\train ->
                { game
                    | world =
                        game.world
                            |> Dict.insert newPos Data.Block.Track
                    , train = train
                }
            )
        |> Maybe.withDefault game


move : Game -> Game
move game =
    let
        newPos =
            Data.Train.forwardPos game.train
    in
    game.train
        |> Data.Train.removeCoal
        |> Maybe.map
            (\train ->
                game
                    |> (\g -> { g | train = train })
                    |> (\g ->
                            g.world
                                |> Dict.insert g.train.pos Data.Block.Track
                                |> Dict.insert newPos Data.Block.Train
                                |> (\world -> { g | world = world })
                       )
                    |> (\g -> { g | train = g.train |> Data.Train.move })
            )
        |> Maybe.withDefault game


mine : Game -> Generator Game
mine game =
    let
        newPos =
            game.train |> Data.Train.forwardPos
    in
    newPos
        :: Data.Position.neighbors newPos
        |> List.foldl
            (\pos ->
                Random.andThen (Data.Game.Wall.mine pos)
            )
            (Random.constant game)
