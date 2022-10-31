module Data.Game.Wagon exposing (..)

import Data.Block
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Position
import Data.Train
import Dict


moveTo : ( Int, Int ) -> ( ( Int, Int ), List Item ) -> Game -> Game
moveTo newWagonPos ( pos, content ) game =
    (case Dict.get newWagonPos game.world of
        Just (Data.Block.Ground maybeItem) ->
            ( game.world
                |> Dict.insert newWagonPos
                    (maybeItem
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                        |> (\l -> l ++ content)
                        |> Data.Block.Wagon
                    )
                |> Dict.insert pos (Data.Block.Ground Nothing)
            , newWagonPos
            )

        _ ->
            case Dict.get game.player.pos game.world of
                Just (Data.Block.Ground Nothing) ->
                    ( game.world
                        |> Dict.insert game.player.pos (Data.Block.Wagon content)
                        |> Dict.insert pos (Data.Block.Ground Nothing)
                    , game.player.pos
                    )

                _ ->
                    ( game.world, pos )
    )
        |> (\( world, p ) -> { game | world = world } |> unload ( p, content ))


unload : ( ( Int, Int ), List Item ) -> Game -> Game
unload ( pos, content ) game =
    if List.member game.train.pos (Data.Position.neighbors pos) then
        content
            |> List.foldl Data.Train.addItem game.train
            |> (\train -> { game | train = train })
            |> (\g ->
                    { g
                        | world =
                            g.world
                                |> Dict.insert pos (Data.Block.Wagon [])
                    }
               )

    else
        game
