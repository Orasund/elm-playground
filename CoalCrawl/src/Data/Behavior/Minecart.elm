module Data.Behavior.Minecart exposing (..)

import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Floor
import Data.Minecart exposing (Minecart)
import Data.Position
import Data.World exposing (World)


act :
    Int
    -> World
    -> Maybe ( World, List Effect )
act id world =
    world
        |> getMinecart id
        |> Maybe.andThen
            (\( pos, wagon ) ->
                world
                    |> move id ( pos, wagon )
            )
        |> Maybe.map
            (\( g, l ) ->
                g
                    |> unload id
                    |> Maybe.withDefault ( g, [] )
                    |> Tuple.mapSecond ((++) l)
            )


move : Int -> ( ( Int, Int ), Minecart ) -> World -> Maybe ( World, List Effect )
move id ( pos, wagon ) world =
    wagon.movedFrom
        |> Maybe.map
            (\movedFrom ->
                let
                    forwardPos =
                        movedFrom
                            |> Data.Position.vecTo pos
                            |> Data.Position.plus pos

                    positions =
                        { backPos = movedFrom
                        , forwardPos = forwardPos
                        }
                in
                world
                    |> (if Data.World.getFloor pos world == Just Data.Floor.Track then
                            moveOnTrack positions ( pos, id, wagon )

                        else
                            moveOnGround positions ( pos, id, wagon )
                       )
                    |> (\( world0, p, effects ) ->
                            ( world0
                                |> Data.World.moveActorTo p id
                            , effects
                            )
                       )
            )


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Int, Minecart ) -> World -> ( World, ( Int, Int ), List Effect )
moveOnGround args ( pos, id, wagon ) world =
    (case Data.World.get args.forwardPos world of
        Just ( Data.Block.FloorBlock _, _ ) ->
            world
                |> collect pos id
                |> (\( w, l ) -> ( w, args.forwardPos, l ))

        Just ( Data.Block.EntityBlock entity, _ ) ->
            ( case entity of
                Data.Entity.Actor id0 ->
                    world
                        |> swapWith id0 ( pos, id )
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
    )
        |> (\( w, p, l ) ->
                ( w
                    |> getMinecart id
                    |> Maybe.map Tuple.second
                    |> Maybe.map Data.Minecart.stop
                    |> Maybe.map Data.Actor.Minecart
                    |> Maybe.map
                        (\actor ->
                            w
                                |> Data.World.setActor id actor
                        )
                    |> Maybe.withDefault w
                , p
                , l
                )
           )


swapWith : Int -> ( ( Int, Int ), Int ) -> World -> Maybe World
swapWith id0 ( pos, id1 ) world =
    Maybe.map2
        (\( _, w0 ) ( _, w1 ) ->
            world
                |> Data.World.setActor id0
                    (w1.storage
                        |> Data.Minecart.setStorageOf w0
                        |> Data.Minecart.moveFrom pos
                        |> Data.Actor.Minecart
                    )
                |> Data.World.setActor id1
                    (w0.storage
                        |> Data.Minecart.setStorageOf w1
                        |> Data.Minecart.stop
                        |> Data.Actor.Minecart
                    )
        )
        (getMinecart id0 world)
        (getMinecart id1 world)


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
                |> getMinecart id
                |> Maybe.map Tuple.second
                |> Maybe.map Data.Actor.Minecart
                |> Maybe.map
                    (\actor ->
                        world
                            |> Data.World.setActor id actor
                    )
                |> Maybe.withDefault world
                |> collect pos id
                |> (\( w, l ) -> ( w, p, l ))

        _ ->
            world
                |> moveOnGround args ( pos, id, wagon )


collect : ( Int, Int ) -> Int -> World -> ( World, List Effect )
collect pos id w =
    pos
        |> Data.Position.neighbors
        |> List.foldl
            (\p ( world, l ) ->
                world
                    |> pickup p id
                    |> Maybe.map (Tuple.mapSecond (\e -> e :: l))
                    |> Maybe.withDefault ( world, l )
            )
            ( w, [] )


pickup : ( Int, Int ) -> Int -> World -> Maybe ( World, Effect )
pickup pos id world =
    world
        |> Data.World.get pos
        |> Maybe.andThen Tuple.second
        |> Maybe.andThen
            (\item ->
                world
                    |> getMinecart id
                    |> Maybe.map Tuple.second
                    |> Maybe.andThen (Data.Minecart.insert item)
                    |> Maybe.map
                        (\( m, s ) ->
                            ( world
                                |> Data.World.updateActor id
                                    (\_ -> Data.Actor.Minecart m)
                                |> Data.World.removeItem pos
                            , Data.Effect.PlaySound s
                            )
                        )
            )


unload : Int -> World -> Maybe ( World, List Effect )
unload id world =
    world
        |> getMinecart id
        |> Maybe.andThen
            (\( pos, _ ) ->
                pos
                    |> Data.Position.neighbors
                    |> List.filter
                        (\p ->
                            case world |> Data.World.getActorAt p of
                                Just ( _, Data.Actor.Train _ ) ->
                                    True

                                _ ->
                                    False
                        )
                    |> List.head
                    |> Maybe.map (\p -> { from = pos, to = p })
            )
        |> Maybe.andThen (\args -> Data.World.transfer args world)


getMinecart : Int -> World -> Maybe ( ( Int, Int ), Minecart )
getMinecart id world =
    world
        |> Data.World.getActor id
        |> Maybe.andThen
            (\( pos, actor ) ->
                case actor of
                    Data.Actor.Minecart minecart ->
                        Just ( pos, minecart )

                    _ ->
                        Nothing
            )
