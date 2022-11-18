module Data.Behavior.Player exposing (..)

import AStar
import Data.Actor
import Data.Behavior.Wagon
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Sound
import Data.Train
import Data.Wagon
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
                            Data.Entity.Train ->
                                putIntoTrain game
                                    |> Random.constant

                            Data.Entity.Wall ->
                                Random.constant game
                                    |> Random.map (\g -> ( g, [] ))

                            Data.Entity.Actor id ->
                                game.world.actors
                                    |> Dict.get id
                                    |> Maybe.map
                                        (\( _, actor ) ->
                                            case actor of
                                                Data.Actor.Wagon _ ->
                                                    game |> putIntoWagon |> Random.constant

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
                Just ( _, Data.Actor.Wagon _ ) ->
                    True

                Just ( _, Data.Actor.Cave _ ) ->
                    False

                Just ( _, Data.Actor.Mine ) ->
                    False

                Just ( _, Data.Actor.Bomb _ ) ->
                    False

                Just ( _, Data.Actor.FallingCoal ) ->
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
                        Just (Data.Actor.Wagon wagon) ->
                            game
                                |> Data.Behavior.Wagon.move { backPos = game.player.pos }
                                    id
                                    ( pos, wagon )
                                |> Tuple.mapFirst
                                    (\g ->
                                        { g | player = g.player |> Data.Player.moveTo pos }
                                    )
                                |> Random.constant

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
        Just ( id, Data.Actor.Wagon wagon ) ->
            (if Data.Wagon.isFull wagon then
                ( game, [ Data.Effect.PlaySound Data.Sound.Error ] )

             else
                game.player
                    |> Data.Player.dropItem
                    |> Maybe.map
                        (\( player, item ) ->
                            wagon
                                |> Data.Wagon.insert item
                                |> Tuple.mapFirst (\w2 -> game.world |> Data.World.updateActor id (\_ -> Data.Actor.Wagon w2))
                                |> Tuple.mapFirst (\world -> { game | world = world })
                                |> (\( g, sound ) ->
                                        ( { g | player = player }
                                        , [ Data.Effect.PlaySound Data.Sound.Unload
                                          , Data.Effect.PlaySound sound
                                          ]
                                        )
                                   )
                        )
                    |> Maybe.withDefault ( game, [] )
            )
                |> (\( g, l ) ->
                        g
                            |> Data.Behavior.Wagon.unload id
                            |> Tuple.mapSecond ((++) l)
                   )

        _ ->
            ( game, [] )


putIntoTrain : Game -> ( Game, List Effect )
putIntoTrain game =
    Data.Player.dropItem game.player
        |> Maybe.map
            (\( player, item ) ->
                { game | player = player }
                    |> (\g -> { g | train = g.train |> Data.Train.addItem item })
                    |> (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.Unload ] ))
            )
        |> Maybe.withDefault ( game, [] )
