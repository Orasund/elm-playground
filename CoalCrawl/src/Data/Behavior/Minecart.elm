module Data.Behavior.Minecart exposing (..)

import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Minecart exposing (Minecart)
import Data.Position
import Data.Sound
import Data.Storage
import Data.Train
import Data.World exposing (World)
import Dict


act :
    { backPos : ( Int, Int ) }
    -> Int
    -> Game
    -> ( Game, List Effect )
act args id game =
    (case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Minecart wagon ) ->
            if Data.World.getFloor pos game.world == Just Data.Floor.Track then
                game.world
                    |> move args id ( pos, wagon )
                    |> Tuple.mapFirst (Data.Game.setWorldOf game)

            else
                ( game, [] )

        _ ->
            ( game, [] )
    )
        |> (\( g, l ) ->
                g
                    |> unload id
                    |> Tuple.mapSecond ((++) l)
           )


move : { backPos : ( Int, Int ) } -> Int -> ( ( Int, Int ), Minecart ) -> World -> ( World, List Effect )
move { backPos } id ( pos, wagon ) world =
    let
        forwardPos =
            backPos
                |> Data.Position.vecTo pos
                |> Data.Position.plus pos

        positions =
            { backPos = backPos, forwardPos = forwardPos }
    in
    (if Data.World.getFloor pos world == Just Data.Floor.Track then
        world
            |> moveOnTrack positions ( pos, id, wagon )

     else
        world
            |> moveOnGround positions ( pos, id, wagon )
    )
        |> (\( world0, p, effects ) ->
                ( world0
                    |> Data.World.moveActorTo p id
                    |> Data.World.updateActor id
                        (\actor ->
                            case actor of
                                Data.Actor.Minecart w ->
                                    Data.Actor.Minecart (w |> Data.Minecart.moveFrom pos)

                                _ ->
                                    actor
                        )
                , effects
                )
           )


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Int, Minecart ) -> World -> ( World, ( Int, Int ), List Effect )
moveOnGround args ( pos, id, wagon ) world =
    case Data.World.get args.forwardPos world of
        Just ( Data.Block.FloorBlock _, _ ) ->
            world
                |> pickup pos id
                |> (\( w, l ) -> ( w, args.forwardPos, l ))

        Just ( Data.Block.EntityBlock entity, _ ) ->
            ( case entity of
                Data.Entity.Actor id0 ->
                    world
                        |> Data.World.getActor id0
                        |> Maybe.map
                            (\( _, actor ) ->
                                case actor of
                                    Data.Actor.Minecart w0 ->
                                        world
                                            |> Data.World.updateActor id0
                                                (\_ ->
                                                    wagon.storage
                                                        |> Data.Minecart.setStorageOf w0
                                                        |> Data.Minecart.moveFrom pos
                                                        |> Data.Actor.Minecart
                                                )
                                            |> Data.World.updateActor id
                                                (\_ ->
                                                    w0.storage
                                                        |> Data.Minecart.setStorageOf wagon
                                                        |> Data.Minecart.stop
                                                        |> Data.Actor.Minecart
                                                )

                                    _ ->
                                        world
                            )
                        |> Maybe.withDefault world

                _ ->
                    world
            , case Data.World.get args.backPos world of
                Just ( Data.Block.FloorBlock _, _ ) ->
                    args.backPos

                _ ->
                    pos
            , []
            )

        Nothing ->
            ( world, pos, [] )


moveOnTrack :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> ( ( Int, Int ), Int, Minecart )
    -> World
    -> ( World, ( Int, Int ), List Effect )
moveOnTrack args ( pos, id, wagon ) world =
    case
        pos
            |> Data.Position.neighbors
            |> List.filter
                (\p ->
                    (p /= args.backPos)
                        && (case Data.World.get p world of
                                Just ( Data.Block.FloorBlock Data.Floor.Track, _ ) ->
                                    True

                                _ ->
                                    False
                           )
                )
    of
        [ p ] ->
            world
                |> pickup pos id
                |> (\( w, l ) -> ( w, p, l ))

        _ ->
            world
                |> moveOnGround args ( pos, id, wagon )


pickup : ( Int, Int ) -> Int -> World -> ( World, List Effect )
pickup pos id w =
    pos
        |> Data.Position.neighbors
        |> List.foldl
            (\p ( world, l ) ->
                world
                    |> Data.World.get p
                    |> Maybe.andThen Tuple.second
                    |> Maybe.map
                        (\item ->
                            case world |> Data.World.getActor id of
                                Just ( _, Data.Actor.Minecart minecart ) ->
                                    minecart
                                        |> Data.Minecart.insert item
                                        |> Maybe.map
                                            (\( m, s ) ->
                                                ( world
                                                    |> Data.World.updateActor id
                                                        (\_ -> Data.Actor.Minecart m)
                                                    |> Data.World.removeItem p
                                                , Data.Effect.PlaySound s :: l
                                                )
                                            )
                                        |> Maybe.withDefault ( world, l )

                                _ ->
                                    ( world, l )
                        )
                    |> Maybe.withDefault ( world, l )
            )
            ( w, [] )


unload : Int -> Game -> ( Game, List Effect )
unload id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Minecart wagon ) ->
            if
                List.member (game |> Data.Game.getTrain |> .pos) (Data.Position.neighbors pos)
                    && (Data.Storage.isEmpty wagon.storage |> not)
            then
                Data.Minecart.unload wagon
                    |> (\( m, anyBag ) ->
                            ( game
                                |> Data.Game.getTrain
                                |> Data.Train.addAll anyBag
                                |> Data.Game.setTrainOf game
                                |> (\g ->
                                        { g
                                            | world =
                                                g.world
                                                    |> Data.World.updateActor id (\_ -> Data.Actor.Minecart m)
                                        }
                                   )
                            , [ Data.Effect.PlaySound Data.Sound.Unload ]
                            )
                       )

            else
                ( game, [] )

        _ ->
            ( game, [] )
