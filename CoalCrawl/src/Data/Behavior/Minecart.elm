module Data.Behavior.Minecart exposing (..)

import AnyBag
import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Minecart exposing (Minecart)
import Data.Position
import Data.Sound
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
                game
                    |> move args id ( pos, wagon )

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


move : { backPos : ( Int, Int ) } -> Int -> ( ( Int, Int ), Minecart ) -> Game -> ( Game, List Effect )
move { backPos } id ( pos, wagon ) game =
    let
        forwardPos =
            backPos
                |> Data.Position.vecTo pos
                |> Data.Position.plus pos

        positions =
            { backPos = backPos, forwardPos = forwardPos }
    in
    (if Data.World.getFloor pos game.world == Just Data.Floor.Track then
        game.world
            |> moveOnTrack positions ( pos, wagon )

     else
        game.world
            |> moveOnGround positions ( pos, wagon )
    )
        |> (\( world, p, effects ) ->
                world
                    |> Data.World.moveActorTo p id
                    |> Data.World.updateActor id
                        (\actor ->
                            case actor of
                                Data.Actor.Minecart w ->
                                    Data.Actor.Minecart (w |> Data.Minecart.moveFrom pos)

                                _ ->
                                    actor
                        )
                    |> (\w -> ( { game | world = w }, effects ))
           )


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Minecart ) -> World -> ( World, ( Int, Int ), List Effect )
moveOnGround args ( pos, wagon ) world =
    case Data.World.get args.forwardPos world of
        Just ( Data.Block.FloorBlock _, maybeItem ) ->
            case maybeItem of
                Just item ->
                    case Data.World.getActorAt pos world of
                        Just ( id, _ ) ->
                            if Data.Minecart.isFull wagon then
                                ( world, args.forwardPos, [] )

                            else
                                wagon
                                    |> Data.Minecart.insert item
                                    |> (\( wag, sound ) ->
                                            world
                                                |> Data.World.updateActor id (\_ -> Data.Actor.Minecart wag)
                                                |> Data.World.removeItem args.forwardPos
                                                |> (\w ->
                                                        ( w
                                                        , args.forwardPos
                                                        , [ Data.Effect.PlaySound sound ]
                                                        )
                                                   )
                                       )

                        _ ->
                            ( world, args.forwardPos, [] )

                Nothing ->
                    ( world, args.forwardPos, [] )

        Just ( Data.Block.EntityBlock entity, _ ) ->
            ( case entity of
                Data.Entity.Actor id0 ->
                    world
                        |> Data.World.getActor id0
                        |> Maybe.map
                            (\( _, actor ) ->
                                case actor of
                                    Data.Actor.Minecart w0 ->
                                        case Data.World.getActorAt pos world of
                                            Just ( id, _ ) ->
                                                world
                                                    |> Data.World.updateActor id0
                                                        (\_ ->
                                                            w0
                                                                |> Data.Minecart.load wagon.items
                                                                |> Data.Minecart.moveFrom pos
                                                                |> Data.Actor.Minecart
                                                        )
                                                    |> Data.World.updateActor id
                                                        (\_ ->
                                                            wagon
                                                                |> Data.Minecart.load w0.items
                                                                |> Data.Minecart.stop
                                                                |> Data.Actor.Minecart
                                                        )

                                            _ ->
                                                world

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
    -> ( ( Int, Int ), Minecart )
    -> World
    -> ( World, ( Int, Int ), List Effect )
moveOnTrack args ( pos, wagon ) world =
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
            ( world, p, [] )

        _ ->
            world
                |> moveOnGround args ( pos, wagon )


unload : Int -> Game -> ( Game, List Effect )
unload id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Minecart wagon ) ->
            if
                List.member game.train.pos (Data.Position.neighbors pos)
                    && (AnyBag.size wagon.items > 0)
            then
                ( { game
                    | train = Data.Train.addAll wagon.items game.train
                    , world =
                        game.world
                            |> Data.World.updateActor id (\_ -> Data.Actor.Minecart (Data.Minecart.unload wagon))
                  }
                , [ Data.Effect.PlaySound Data.Sound.Unload ]
                )

            else
                ( game, [] )

        _ ->
            ( game, [] )
