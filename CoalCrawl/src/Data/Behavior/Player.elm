module Data.Behavior.Player exposing (..)

import AStar
import Data.Actor
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Sound
import Data.World
import Dict
import Generation
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
                        moveTowards pos game

                    Data.Block.EntityBlock entity ->
                        case entity of
                            Data.Entity.Wall ->
                                Random.constant game
                                    |> Random.map (\g -> ( g, [] ))

                            Data.Entity.Actor id ->
                                game.world.actors
                                    |> Dict.get id
                                    |> Maybe.andThen (\( p, _ ) -> putInto p game)
                                    |> Maybe.withDefault ( game, [] )
                                    |> Random.constant

                            Data.Entity.Vein _ ->
                                game.world
                                    |> Generation.mine pos
                                    |> Random.map (\world -> ( { game | world = world }, [] ))
                                    |> Data.Effect.map (pickUp pos)

                            Data.Entity.Water ->
                                Random.constant ( game, [] )

                            Data.Entity.Lava ->
                                Random.constant ( game, [] )
            )
        |> Maybe.withDefault (Random.constant ( game, [] ))


canMoveTo : Game -> ( Int, Int ) -> Bool
canMoveTo game p =
    case Data.World.get p game.world of
        Just ( Data.Block.FloorBlock _, _ ) ->
            True

        Just ( Data.Block.EntityBlock (Data.Entity.Actor id), _ ) ->
            case game.world |> Data.World.getActor id of
                Just ( _, Data.Actor.Minecart _ ) ->
                    True

                Just ( _, Data.Actor.MovingWater _ ) ->
                    True

                Just ( _, Data.Actor.Train _ ) ->
                    True

                _ ->
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
            case Data.World.getBlock pos game.world of
                Just (Data.Block.FloorBlock _) ->
                    { game | player = game.player |> Data.Player.moveTo pos }
                        |> Data.Effect.withNone

                Just (Data.Block.EntityBlock (Data.Entity.Actor id)) ->
                    case game.world |> Data.World.getActor id of
                        Just ( _, Data.Actor.Train _ ) ->
                            { game | player = game.player |> Data.Player.moveTo pos }
                                |> Data.Effect.withNone

                        _ ->
                            game.world
                                |> Data.World.push { from = game.player.pos, pos = pos }
                                |> Maybe.map (Data.Game.setWorldOf game)
                                |> Maybe.withDefault game
                                |> Data.Effect.withNone

                Just (Data.Block.EntityBlock (Data.Entity.Vein _)) ->
                    game.world
                        |> Generation.mine pos
                        |> Random.map (Data.Game.setWorldOf game)
                        |> Data.Effect.genWithNone

                _ ->
                    game.world
                        |> Data.World.push { from = game.player.pos, pos = pos }
                        |> Maybe.map (Data.Game.setWorldOf game)
                        |> Maybe.withDefault game
                        |> Data.Effect.withNone

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


putInto : ( Int, Int ) -> Game -> Maybe ( Game, List Effect )
putInto pos game =
    Data.Player.dropItem game.player
        |> Maybe.andThen
            (\( player, item ) ->
                game.world
                    |> Data.World.load pos [ item ]
                    |> Maybe.map
                        (Tuple.mapFirst
                            (\w ->
                                { game
                                    | world = w
                                    , player = player
                                }
                            )
                        )
            )
