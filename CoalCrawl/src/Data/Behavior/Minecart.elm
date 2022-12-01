module Data.Behavior.Minecart exposing (..)

import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Floor
import Data.Minecart exposing (Minecart)
import Data.Position
import Data.World exposing (World, transfer)
import Random exposing (Generator)


act :
    Int
    -> World
    -> Maybe (Generator ( World, List Effect ))
act id world =
    world
        |> getMinecart id
        |> Maybe.andThen
            (\( pos, wagon ) ->
                world
                    |> move id ( pos, wagon )
                    |> Maybe.map
                        (Data.Effect.map (collect pos id))
            )
        |> Maybe.map
            (Data.Effect.map
                (\g ->
                    g
                        |> unload id
                        |> Maybe.withDefault ( g, [] )
                )
            )


move : Int -> ( ( Int, Int ), Minecart ) -> World -> Maybe (Generator ( World, List Effect ))
move id ( pos, wagon ) world =
    wagon.movedFrom
        |> Maybe.map
            (\movedFrom ->
                { backPos = movedFrom
                , forwardPos =
                    movedFrom
                        |> Data.Position.vecTo pos
                        |> Data.Position.plus pos
                }
                    |> (\positions ->
                            world
                                |> (if Data.World.getFloor pos world == Just Data.Floor.Track then
                                        moveOnTrack

                                    else
                                        moveOnGround
                                   )
                                    positions
                                    ( pos, id, wagon )
                       )
            )


setMovement : ( Int, ( Int, Int ) ) -> ( Int, Int ) -> World -> World
setMovement ( id, pos ) p w =
    w
        |> getMinecart id
        |> Maybe.map Tuple.second
        |> Maybe.map
            (if
                Data.World.getFloor p w
                    == Just Data.Floor.Track
             then
                Data.Minecart.moveFrom pos

             else
                Data.Minecart.stop
            )
        |> Maybe.map Data.Actor.Minecart
        |> Maybe.map
            (\actor ->
                w
                    |> Data.World.setActor id actor
                    |> Data.World.moveActorTo p id
            )
        |> Maybe.withDefault w


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Int, Minecart ) -> World -> Generator ( World, List Effect )
moveOnGround args ( pos, id, wagon ) world =
    world
        |> Data.World.get args.forwardPos
        |> Maybe.map
            (\block ->
                case block of
                    ( Data.Block.FloorBlock _, _ ) ->
                        world
                            |> setMovement ( id, pos ) args.forwardPos
                            |> Data.Effect.withNone

                    ( Data.Block.EntityBlock entity, _ ) ->
                        (case entity of
                            Data.Entity.Actor id0 ->
                                world
                                    |> collideWith id0 ( pos, id )

                            _ ->
                                Nothing
                        )
                            |> Maybe.withDefault ( world, [] )
                            |> (\( w, l ) ->
                                    ( w
                                        |> setMovement ( id, pos )
                                            (case Data.World.get args.backPos world of
                                                Just ( Data.Block.FloorBlock _, _ ) ->
                                                    args.backPos

                                                _ ->
                                                    pos
                                            )
                                    , l
                                    )
                                        |> Random.constant
                               )
            )
        |> Maybe.withDefault (world |> Data.Effect.withNone)


collideWith : Int -> ( ( Int, Int ), Int ) -> World -> Maybe ( World, List Effect )
collideWith target ( pos, id1 ) world =
    world
        |> Data.World.getActor target
        |> Maybe.andThen
            (\( from, actor ) ->
                case actor of
                    Data.Actor.Minecart _ ->
                        world
                            |> swapWith target ( pos, id1 )
                            |> Maybe.map (\w -> ( w, [] ))

                    _ ->
                        world
                            |> transfer { from = from, to = pos }
            )


swapWith : Int -> ( ( Int, Int ), Int ) -> World -> Maybe World
swapWith id0 ( pos, id1 ) world =
    Maybe.map2
        (\( p0, w0 ) ( _, w1 ) ->
            world
                |> Data.World.setActor id0
                    (w1.storage
                        |> Data.Minecart.setStorageOf w0
                        |> (if Data.World.getFloor p0 world == Just Data.Floor.Track then
                                Data.Minecart.moveFrom pos

                            else
                                identity
                           )
                        |> Data.Actor.Minecart
                    )
                |> Data.World.setActor id1
                    (w0.storage
                        |> Data.Minecart.setStorageOf w1
                        |> Data.Actor.Minecart
                    )
        )
        (getMinecart id0 world)
        (getMinecart id1 world)


moveOnTrack :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> ( ( Int, Int ), Int, Minecart )
    -> World
    -> Generator ( World, List Effect )
moveOnTrack args ( pos, id, wagon ) world =
    case
        world
            |> neighborTracks pos
            |> List.filter (\p -> p /= args.backPos)
    of
        head :: tail ->
            Random.uniform head tail
                |> Random.andThen
                    (\p ->
                        world
                            |> setMovement ( id, pos ) p
                            |> Data.Effect.withNone
                    )

        [] ->
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
                    |> Maybe.map (Tuple.mapSecond (\e -> e ++ l))
                    |> Maybe.withDefault ( world, l )
            )
            ( w, [] )


pickup : ( Int, Int ) -> Int -> World -> Maybe ( World, List Effect )
pickup from id world =
    world
        |> Data.World.get from
        |> Maybe.andThen
            (\( block, maybeItem ) ->
                case block of
                    Data.Block.FloorBlock _ ->
                        Maybe.map2
                            Data.Minecart.insert
                            maybeItem
                            (world
                                |> getMinecart id
                                |> Maybe.map Tuple.second
                            )
                            |> Maybe.andThen identity
                            |> Maybe.map
                                (Tuple.mapBoth
                                    (\m ->
                                        world
                                            |> Data.World.updateActor id
                                                (\_ -> Data.Actor.Minecart m)
                                            |> Data.World.removeItem from
                                    )
                                    (\s ->
                                        s
                                            |> Data.Effect.PlaySound
                                            |> List.singleton
                                    )
                                )

                    Data.Block.EntityBlock (Data.Entity.Actor targetId) ->
                        Maybe.map2
                            (\actor to ->
                                case actor of
                                    Data.Actor.Excavator _ ->
                                        world
                                            |> Data.World.transfer { from = from, to = to }

                                    _ ->
                                        Nothing
                            )
                            (world
                                |> Data.World.getActor targetId
                                |> Maybe.map Tuple.second
                            )
                            (world
                                |> getMinecart id
                                |> Maybe.map Tuple.first
                            )
                            |> Maybe.andThen identity

                    _ ->
                        Nothing
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


neighborTracks : ( Int, Int ) -> World -> List ( Int, Int )
neighborTracks pos world =
    pos
        |> Data.Position.neighbors
        |> List.filter
            (\p ->
                case Data.World.get p world of
                    Just ( Data.Block.FloorBlock Data.Floor.Track, _ ) ->
                        True

                    _ ->
                        False
            )


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
