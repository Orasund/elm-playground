module Data.Behavior.Wagon exposing (..)

import AnyBag exposing (AnyBag)
import Data.Block
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Position
import Data.Train
import Data.World exposing (World)
import Random exposing (Generator)


moveTo : ( Int, Int ) -> ( ( Int, Int ), AnyBag String Item ) -> Game -> Generator Game
moveTo newWagonPos ( pos, content ) game =
    (if Data.World.getFloor pos game.world == Just Data.Floor.Track then
        moveOnTrack newWagonPos ( pos, content ) game

     else
        (case Data.World.get newWagonPos game.world of
            Just (Data.Block.FloorBlock (Data.Floor.Ground maybeItem)) ->
                ( game.world
                    |> Data.World.insertEntity newWagonPos
                        (maybeItem
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                            |> (\l -> l |> List.foldl (AnyBag.insert 1) content)
                            |> Data.Entity.Wagon
                        )
                    |> Data.World.removeEntity pos
                , newWagonPos
                )

            Just (Data.Block.FloorBlock Data.Floor.Track) ->
                ( game.world
                    |> Data.World.insertEntity newWagonPos (Data.Entity.Wagon content)
                    |> Data.World.removeEntity pos
                , newWagonPos
                )

            _ ->
                case Data.World.get game.player.pos game.world of
                    Just (Data.Block.FloorBlock (Data.Floor.Ground Nothing)) ->
                        ( game.world
                            |> Data.World.insertEntity game.player.pos (Data.Entity.Wagon content)
                            |> Data.World.removeEntity pos
                        , game.player.pos
                        )

                    _ ->
                        ( game.world, pos )
        )
            |> Random.constant
    )
        |> Random.map (\( world, p ) -> { game | world = world } |> unload ( p, content ))


moveOnTrack : ( Int, Int ) -> ( ( Int, Int ), AnyBag String Item ) -> Game -> Generator ( World, ( Int, Int ) )
moveOnTrack alternativePos ( pos, content ) game =
    case
        pos
            |> Data.Position.neighbors
            |> List.filter
                (\p ->
                    (p /= game.player.pos)
                        && (Data.World.get p game.world == Just (Data.Block.FloorBlock Data.Floor.Track))
                )
    of
        head :: tail ->
            Random.uniform head tail
                |> Random.map
                    (\p ->
                        game.world
                            |> Data.World.insertEntity p (Data.Entity.Wagon content)
                            |> Data.World.removeEntity pos
                            |> (\world -> ( world, p ))
                    )

        [] ->
            game.world
                |> Data.World.insertEntity alternativePos (Data.Entity.Wagon content)
                |> Data.World.removeEntity pos
                |> (\world -> ( world, alternativePos ))
                |> Random.constant


unload : ( ( Int, Int ), AnyBag String Item ) -> Game -> Game
unload ( pos, content ) game =
    if List.member game.train.pos (Data.Position.neighbors pos) then
        Data.Train.addAll content game.train
            |> (\train -> { game | train = train })
            |> (\g ->
                    { g
                        | world =
                            g.world
                                |> Data.World.insertEntity pos (Data.Entity.Wagon (AnyBag.empty Data.Item.toString))
                    }
               )

    else
        game
