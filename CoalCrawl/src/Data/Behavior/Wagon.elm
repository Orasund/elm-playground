module Data.Behavior.Wagon exposing (..)

import AnyBag
import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Position
import Data.Sound
import Data.Train
import Data.Wagon exposing (Wagon)
import Data.World exposing (World)
import Dict


act :
    { backPos : ( Int, Int ) }
    -> Int
    -> Game
    -> ( Game, List Effect )
act args id game =
    (case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            if Data.World.getFloor pos game.world == Just Data.Floor.Track then
                game
                    |> move args id ( pos, wagon )

            else
                game

        _ ->
            game
    )
        |> unload id


move : { backPos : ( Int, Int ) } -> Int -> ( ( Int, Int ), Wagon ) -> Game -> Game
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
        |> (\( world, p ) ->
                world
                    |> Data.World.moveActorTo p id
                    |> Data.World.updateActor id
                        (\actor ->
                            case actor of
                                Data.Actor.Wagon w ->
                                    Data.Actor.Wagon (w |> Data.Wagon.moveFrom pos)

                                _ ->
                                    actor
                        )
           )
        |> (\world -> { game | world = world })


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Wagon ) -> World -> ( World, ( Int, Int ) )
moveOnGround args ( pos, wagon ) world =
    case Data.World.get args.forwardPos world of
        Just (Data.Block.FloorBlock floor) ->
            case Data.World.get pos world of
                Just (Data.Block.EntityBlock (Data.Entity.Actor id)) ->
                    case floor of
                        Data.Floor.Ground (Just item) ->
                            if Data.Wagon.isFull wagon then
                                ( world, args.forwardPos )

                            else
                                world
                                    |> Data.World.updateActor id
                                        (\_ ->
                                            wagon
                                                |> Data.Wagon.insert item
                                                |> Data.Actor.Wagon
                                        )
                                    |> Data.World.insertFloor Data.Floor.ground pos
                                    |> (\w -> ( w, args.forwardPos ))

                        _ ->
                            ( world, args.forwardPos )

                _ ->
                    ( world, args.forwardPos )

        Just (Data.Block.EntityBlock entity) ->
            ( case entity of
                Data.Entity.Actor id0 ->
                    world
                        |> Data.World.getActor id0
                        |> Maybe.map
                            (\( _, actor ) ->
                                case actor of
                                    Data.Actor.Wagon w0 ->
                                        case Data.World.get pos world of
                                            Just (Data.Block.EntityBlock (Data.Entity.Actor id)) ->
                                                world
                                                    |> Data.World.updateActor id0
                                                        (\_ ->
                                                            w0
                                                                |> Data.Wagon.load wagon.items
                                                                |> Data.Wagon.moveFrom pos
                                                                |> Data.Actor.Wagon
                                                        )
                                                    |> Data.World.updateActor id
                                                        (\_ ->
                                                            wagon
                                                                |> Data.Wagon.load w0.items
                                                                |> Data.Wagon.stop
                                                                |> Data.Actor.Wagon
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
                Just (Data.Block.FloorBlock _) ->
                    args.backPos

                _ ->
                    pos
            )

        Nothing ->
            ( world, pos )


moveOnTrack :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> ( ( Int, Int ), Wagon )
    -> World
    -> ( World, ( Int, Int ) )
moveOnTrack args ( pos, wagon ) world =
    case
        pos
            |> Data.Position.neighbors
            |> List.filter
                (\p ->
                    (p /= args.backPos)
                        && (Data.World.get p world == Just (Data.Block.FloorBlock Data.Floor.Track))
                )
    of
        [ p ] ->
            ( world, p )

        _ ->
            world
                |> moveOnGround args ( pos, wagon )


unload : Int -> Game -> ( Game, List Effect )
unload id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            if
                List.member game.train.pos (Data.Position.neighbors pos)
                    && (AnyBag.size wagon.items > 0)
            then
                ( { game
                    | train = Data.Train.addAll wagon.items game.train
                    , world =
                        game.world
                            |> Data.World.updateActor id (\_ -> Data.Actor.Wagon (Data.Wagon.unload wagon))
                  }
                , [ Data.Effect.PlaySound Data.Sound.Unload ]
                )

            else
                ( game, [] )

        _ ->
            ( game, [] )
