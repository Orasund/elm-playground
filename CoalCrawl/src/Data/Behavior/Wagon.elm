module Data.Behavior.Wagon exposing (..)

import Data.Block
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Train
import Data.Wagon exposing (Wagon)
import Data.World
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
        moveOnGround args ( pos, wagon ) game
            |> Random.constant
    )
        |> Random.map (\( g, p ) -> g |> unload ( p, wagon ))


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Wagon ) -> Game -> ( Game, ( Int, Int ) )
moveOnGround args ( pos, wagon ) game =
    case Data.World.get args.forwardPos game.world of
        Just (Data.Block.FloorBlock _) ->
            ( game.world
                |> Data.World.insertEntity args.forwardPos (Data.Entity.Wagon wagon)
                |> Data.World.removeEntity pos
                |> (\world -> { game | world = world })
            , args.forwardPos
            )

        _ ->
            case Data.World.get args.backPos game.world of
                Just (Data.Block.FloorBlock _) ->
                    ( game.world
                        |> Data.World.insertEntity args.backPos (Data.Entity.Wagon wagon)
                        |> Data.World.removeEntity pos
                        |> (\world -> { game | world = world })
                    , args.backPos
                    )

                _ ->
                    ( game, pos )


moveOnTrack :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> ( ( Int, Int ), Wagon )
    -> Game
    -> Generator ( Game, ( Int, Int ) )
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
                            |> (\world ->
                                    ( { game
                                        | world = world
                                        , player =
                                            game.player
                                                |> Data.Player.startRiding
                                                |> Data.Player.moveTo pos
                                      }
                                    , p
                                    )
                               )
                    )

        [] ->
            game
                |> moveOnGround args ( pos, wagon |> Data.Wagon.stop )
                |> (\( g, p ) ->
                        ( { g
                            | player =
                                g.player
                                    |> Data.Player.stopRiding
                                    |> Data.Player.moveTo p
                          }
                        , p
                        )
                   )
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
