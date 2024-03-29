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
import Generation
import Random exposing (Generator)
import Set


act : Game -> Generator ( Game, List Effect )
act game =
    game.player.targetPos
        |> Maybe.map
            (\targetPos ->
                (if
                    (Data.Position.neighbors game.player.pos |> List.member targetPos)
                        || (targetPos == game.player.pos)
                 then
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

                    Data.Block.ActorBlock ( _, _ ) ->
                        game
                            |> putInto pos
                            |> Maybe.withDefault ( game, [] )
                            |> Random.constant

                    Data.Block.EntityBlock entity ->
                        case entity of
                            Data.Entity.Wall ->
                                Random.constant game
                                    |> Random.map (\g -> ( g, [] ))

                            Data.Entity.CrackedWall ->
                                Random.constant game
                                    |> Random.map (\g -> ( g, [] ))

                            Data.Entity.Container _ ->
                                game
                                    |> putInto pos
                                    |> Maybe.withDefault ( game, [] )
                                    |> Random.constant

                            Data.Entity.Vein _ ->
                                game.world
                                    |> Generation.mine pos
                                    |> Random.map (\world -> ( { game | world = world }, [] ))

                            Data.Entity.Water ->
                                Random.constant ( game, [] )

                            Data.Entity.Lava ->
                                Random.constant ( game, [] )
            )
        |> Maybe.map (Data.Effect.map (pickUp pos))
        |> Maybe.withDefault (Random.constant ( game, [] ))


canMoveTo : Game -> ( Int, Int ) -> Bool
canMoveTo game p =
    case Data.World.get p game.world |> Maybe.map Tuple.first of
        Just (Data.Block.FloorBlock _) ->
            True

        Just (Data.Block.ActorBlock ( _, actor )) ->
            case actor of
                Data.Actor.Minecart _ ->
                    True

                Data.Actor.MovingWater _ ->
                    True

                Data.Actor.Train _ ->
                    True

                _ ->
                    False

        Just (Data.Block.EntityBlock Data.Entity.Water) ->
            True

        Just (Data.Block.EntityBlock (Data.Entity.Container _)) ->
            True

        Just (Data.Block.EntityBlock (Data.Entity.Vein _)) ->
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

                Just (Data.Block.ActorBlock ( _, actor )) ->
                    case actor of
                        Data.Actor.Train _ ->
                            { game | player = game.player |> Data.Player.moveTo pos }
                                |> Data.Effect.withNone

                        _ ->
                            game.world
                                |> Data.World.push { from = game.player.pos, pos = pos }
                                |> Maybe.map (Data.Game.setWorldOf game)
                                |> Maybe.withDefault game
                                |> Data.Effect.withNone

                Just (Data.Block.EntityBlock (Data.Entity.Container _)) ->
                    { game | player = game.player |> Data.Player.moveTo pos }
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
