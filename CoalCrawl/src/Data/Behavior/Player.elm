module Data.Behavior.Player exposing (..)

import AStar
import Data.Actor
import Data.Behavior.Minecart
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Game exposing (Game)
import Data.Minecart
import Data.Player
import Data.Position
import Data.Sound
import Data.Train
import Data.World
import Data.World.Generation
import Dict
import Random exposing (Generator)
import Set


act : Game -> Generator ( Game, List Effect )
act game =
    game.player.targetPos
        |> Maybe.map
            (\targetPos ->
                (if Data.Position.neighbors game.player.pos |> List.member targetPos then
                    game
                        |> interactWith targetPos
                        |> Random.map (Tuple.mapFirst (\g -> { g | player = g.player |> Data.Player.stopMoving }))

                 else
                    moveTowards targetPos game
                )
                    |> Random.map (\( g, l ) -> pickUp g.player.pos g |> Tuple.mapSecond ((++) l))
            )
        |> Maybe.withDefault (Random.constant ( game, [] ))


interactWith : ( Int, Int ) -> Game -> Generator ( Game, List Effect )
interactWith pos game =
    game.world
        |> Data.World.getBlock pos
        |> Maybe.map
            (\block ->
                case block of
                    Data.Block.FloorBlock _ ->
                        moveTowards game.selected game

                    Data.Block.EntityBlock entity ->
                        case entity of
                            Data.Entity.Wall ->
                                Random.constant game
                                    |> Random.map (\g -> ( g, [] ))

                            Data.Entity.Actor id ->
                                game.world.actors
                                    |> Dict.get id
                                    |> Maybe.map
                                        (\( _, actor ) ->
                                            case actor of
                                                Data.Actor.Minecart _ ->
                                                    game |> putIntoWagon |> Random.constant

                                                Data.Actor.Excavator _ ->
                                                    Random.constant game
                                                        |> Random.map (\g -> ( g, [] ))

                                                Data.Actor.Train _ ->
                                                    putIntoTrain game
                                                        |> Random.constant

                                                _ ->
                                                    Random.constant game
                                                        |> Random.map (\g -> ( g, [] ))
                                        )
                                    |> Maybe.withDefault (Random.constant ( game, [] ))

                            Data.Entity.Vein _ ->
                                game.world
                                    |> Data.World.Generation.mine game.selected
                                    |> Random.map (\world -> ( { game | world = world }, [ Data.Effect.PlaySound Data.Sound.Mine ] ))
                                    |> Data.Effect.andThen (moveTowards game.selected)

                            Data.Entity.Water ->
                                Random.constant ( game, [] )

                            Data.Entity.Lava ->
                                Random.constant ( game, [] )
            )
        |> Maybe.withDefault (Random.constant ( game, [] ))


walkThroughWater : ( Int, Int ) -> Game -> Game
walkThroughWater pos game =
    let
        forwardPos =
            game.player.pos
                |> Data.Position.vecTo pos
                |> Data.Position.plus pos
    in
    (case game.world |> Data.World.get forwardPos of
        Just ( FloorBlock _, _ ) ->
            forwardPos

        Just ( EntityBlock Data.Entity.Lava, _ ) ->
            forwardPos

        _ ->
            pos
                |> Data.Position.neighbors
                |> List.filter
                    (\p ->
                        case game.world |> Data.World.get p of
                            Just ( FloorBlock _, _ ) ->
                                p /= game.player.pos

                            _ ->
                                False
                    )
                |> (\list ->
                        case list of
                            [ p ] ->
                                p

                            _ ->
                                pos
                   )
    )
        |> (\p ->
                game.world
                    |> Data.World.removeEntity pos
                    |> Data.World.insertEntityAt p Data.Entity.Water
           )
        |> (\world -> { game | world = world, player = game.player |> Data.Player.moveTo pos })


canMoveTo : Game -> ( Int, Int ) -> Bool
canMoveTo game p =
    case Data.World.get p game.world of
        Just ( Data.Block.FloorBlock _, _ ) ->
            True

        Just ( Data.Block.EntityBlock (Data.Entity.Actor id), _ ) ->
            case game.world |> Data.World.getActor id of
                Just ( _, Data.Actor.Minecart _ ) ->
                    True

                Just ( _, Data.Actor.Excavator _ ) ->
                    True

                Just ( _, Data.Actor.Helper _ ) ->
                    False

                Just ( _, Data.Actor.Bomb _ ) ->
                    False

                Just ( _, Data.Actor.Train _ ) ->
                    False

                Just ( _, Data.Actor.WaterSource ) ->
                    False

                Nothing ->
                    False

        Just ( Data.Block.EntityBlock Data.Entity.Water, _ ) ->
            True

        Just ( Data.Block.EntityBlock (Data.Entity.Vein _), _ ) ->
            True

        _ ->
            False


moveTowards : ( Int, Int ) -> Game -> Generator ( Game, List Effect )
moveTowards targetPos game =
    case
        AStar.findPath AStar.straightLineCost
            (\pos ->
                Data.Position.neighbors pos
                    |> List.filter (\p -> (p == targetPos) || canMoveTo game p)
                    |> Set.fromList
            )
            game.player.pos
            targetPos
            |> Maybe.andThen List.head
    of
        Just pos ->
            case Data.World.get pos game.world of
                Just ( Data.Block.EntityBlock (Data.Entity.Actor id), _ ) ->
                    case game.world.actors |> Dict.get id |> Maybe.map Tuple.second of
                        Just (Data.Actor.Minecart minecart) ->
                            game.world
                                |> Data.World.setActor id
                                    ({ minecart | movedFrom = Just game.player.pos }
                                        |> Data.Actor.Minecart
                                    )
                                |> Data.Game.setWorldOf game
                                |> Data.Effect.withNone

                        Just (Data.Actor.Excavator excavator) ->
                            game.world
                                |> Data.World.setActor id
                                    ({ excavator
                                        | momentum =
                                            game.player.pos
                                                |> Data.Position.vecTo pos
                                                |> Just
                                        , hasReversed = False
                                     }
                                        |> Data.Actor.Excavator
                                    )
                                |> (\world -> { game | world = world })
                                |> Data.Effect.withNone

                        _ ->
                            ( game, [] ) |> Random.constant

                Just ( Data.Block.EntityBlock Data.Entity.Water, _ ) ->
                    game
                        |> walkThroughWater pos
                        |> (\g -> ( g, [] ))
                        |> Random.constant

                Just ( Data.Block.EntityBlock (Data.Entity.Vein _), _ ) ->
                    game.world
                        |> Data.World.Generation.mine pos
                        |> Random.map
                            (\world ->
                                ( { game | world = world }
                                , [ Data.Effect.PlaySound Data.Sound.Mine ]
                                )
                            )
                        |> Data.Effect.andThen (moveTowards targetPos)

                Just ( Data.Block.FloorBlock _, _ ) ->
                    { game | player = game.player |> Data.Player.moveTo pos }
                        |> (\g -> ( g, [] ))
                        |> Random.constant

                _ ->
                    ( game, [] ) |> Random.constant

        Nothing ->
            Random.constant ( game, [] )


pickUp : ( Int, Int ) -> Game -> ( Game, List Effect )
pickUp pos game =
    case Data.World.get pos game.world of
        Just ( _, Just item ) ->
            game.player
                |> Data.Player.hold item
                |> Maybe.map
                    (\player ->
                        ( { game
                            | player = player
                            , world =
                                game.world
                                    |> Data.World.removeItem pos
                          }
                        , [ Data.Effect.PlaySound Data.Sound.PickUp ]
                        )
                    )
                |> Maybe.withDefault ( game, [] )

        _ ->
            ( game, [] )


putIntoWagon : Game -> ( Game, List Effect )
putIntoWagon game =
    case Data.World.getActorAt game.selected game.world of
        Just ( id, Data.Actor.Minecart wagon ) ->
            (game.player
                |> Data.Player.dropItem
                |> Maybe.map
                    (\( player, item ) ->
                        wagon
                            |> Data.Minecart.insert item
                            |> Maybe.map
                                (\t ->
                                    t
                                        |> Tuple.mapFirst
                                            (\w2 ->
                                                game.world
                                                    |> Data.World.updateActor id
                                                        (\_ -> Data.Actor.Minecart w2)
                                            )
                                        |> Tuple.mapFirst (\world -> { game | world = world })
                                        |> (\( g, sound ) ->
                                                ( { g | player = player }
                                                , [ Data.Effect.PlaySound Data.Sound.Unload
                                                  , Data.Effect.PlaySound sound
                                                  ]
                                                )
                                           )
                                )
                            |> Maybe.withDefault ( game, [ Data.Effect.PlaySound Data.Sound.Error ] )
                    )
                |> Maybe.withDefault ( game, [] )
            )
                |> (\( g, l ) ->
                        g
                            |> Data.Behavior.Minecart.unload id
                            |> Tuple.mapSecond ((++) l)
                   )

        _ ->
            ( game, [] )


putIntoTrain : Game -> ( Game, List Effect )
putIntoTrain game =
    Data.Player.dropItem game.player
        |> Maybe.map
            (\( player, item ) ->
                game
                    |> Data.Game.getTrain
                    |> Data.Train.addItem item
                    |> Data.Game.setTrainOf game
                    |> (\g -> { g | player = player })
                    |> (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.Unload ] ))
            )
        |> Maybe.withDefault ( game, [] )
