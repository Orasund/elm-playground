module Data.Behavior.Wagon exposing (..)

import Data.Block
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Position
import Data.Train
import Data.Wagon exposing (Wagon)
import Data.World exposing (World)
import Random exposing (Generator)


move :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> ( ( Int, Int ), Wagon )
    -> Game
    -> Generator Game
move args ( pos, wagon ) game =
    (if Data.World.getFloor pos game.world == Just Data.Floor.Track then
        moveOnTrack args ( pos, wagon ) game

     else
        moveOnGround args.forwardPos ( pos, wagon ) game
            |> Random.constant
    )
        |> Random.map (\( world, p ) -> { game | world = world } |> unload ( p, wagon ))


moveOnGround : ( Int, Int ) -> ( ( Int, Int ), Wagon ) -> Game -> ( World, ( Int, Int ) )
moveOnGround newWagonPos ( pos, wagon ) game =
    case Data.World.get newWagonPos game.world of
        Just (Data.Block.FloorBlock _) ->
            ( game.world
                |> Data.World.insertEntity newWagonPos (Data.Entity.Wagon wagon)
                |> Data.World.removeEntity pos
            , newWagonPos
            )

        _ ->
            case Data.World.get game.player.pos game.world of
                Just (Data.Block.FloorBlock _) ->
                    ( game.world
                        |> Data.World.insertEntity game.player.pos (Data.Entity.Wagon wagon)
                        |> Data.World.removeEntity pos
                    , game.player.pos
                    )

                _ ->
                    ( game.world, pos )


moveOnTrack :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> ( ( Int, Int ), Wagon )
    -> Game
    -> Generator ( World, ( Int, Int ) )
moveOnTrack args ( pos, wagon ) game =
    case
        pos
            |> Data.Position.neighbors
            |> List.filter
                (\p ->
                    (p /= args.backPos)
                        && (Data.World.get p game.world == Just (Data.Block.FloorBlock Data.Floor.Track))
                )
    of
        head :: tail ->
            Random.uniform head tail
                |> Random.map
                    (\p ->
                        game.world
                            |> Data.World.insertEntity p
                                (wagon
                                    |> Data.Wagon.moveFrom pos
                                    |> Data.Entity.Wagon
                                )
                            |> Data.World.removeEntity pos
                            |> (\world -> ( world, p ))
                    )

        [] ->
            moveOnGround args.forwardPos ( pos, wagon |> Data.Wagon.stop ) game
                |> Random.constant


unload : ( ( Int, Int ), Wagon ) -> Game -> Game
unload ( pos, wagon ) game =
    if List.member game.train.pos (Data.Position.neighbors pos) then
        Data.Train.addAll wagon.items game.train
            |> (\train -> { game | train = train })
            |> (\g ->
                    { g
                        | world =
                            g.world
                                |> Data.World.insertEntity pos (Data.Entity.Wagon (Data.Wagon.unload wagon))
                    }
               )

    else
        game
