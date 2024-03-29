module Data.Behavior.Minecart exposing (..)

import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Effect exposing (Effect)
import Data.Floor
import Data.Improvement exposing (Improvement)
import Data.Minecart exposing (Minecart)
import Data.Position
import Data.World exposing (World, transfer)
import Random exposing (Generator)


act :
    Int
    -> List Improvement
    -> World
    -> Generator ( World, List Effect )
act id improvements world =
    world
        |> getMinecart id
        |> Maybe.andThen
            (\( pos, wagon ) ->
                world
                    |> move id ( pos, wagon )
                    |> Maybe.map (Data.Effect.map (collect id improvements))
            )
        |> Maybe.withDefault (Data.Effect.withNone world)
        |> Data.Effect.map
            (\g ->
                g
                    |> unload id
                    |> Maybe.withDefault ( g, [] )
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
                                |> (case Data.World.getFloor pos world of
                                        Just Data.Floor.Track ->
                                            moveOnTrack

                                        Just Data.Floor.RailwayTrack ->
                                            moveOnTrack

                                        _ ->
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
            (case Data.World.getFloor p w of
                Just Data.Floor.Track ->
                    Data.Minecart.moveFrom pos

                Just Data.Floor.RailwayTrack ->
                    Data.Minecart.moveFrom pos

                _ ->
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
        |> Maybe.map Tuple.first
        |> Maybe.map
            (\block ->
                case block of
                    Data.Block.FloorBlock _ ->
                        world
                            |> setMovement ( id, pos ) args.forwardPos
                            |> Data.Effect.withNone

                    _ ->
                        world
                            |> transfer { from = pos, to = args.forwardPos }
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


collect : Int -> List Improvement -> World -> ( World, List Effect )
collect id improvements w =
    w
        |> getMinecart id
        |> Maybe.map
            (\( pos, _ ) ->
                pos
                    |> Data.Position.neighbors
                    |> List.foldl
                        (\p ( world, l ) ->
                            world
                                |> pickup { from = p, to = pos, improvements = improvements } id
                                |> Maybe.map (Tuple.mapSecond (\e -> e ++ l))
                                |> Maybe.withDefault ( world, l )
                        )
                        ( w, [] )
            )
        |> Maybe.withDefault ( w, [] )


pickup : { from : ( Int, Int ), to : ( Int, Int ), improvements : List Improvement } -> Int -> World -> Maybe ( World, List Effect )
pickup args id world =
    world
        |> Data.World.get args.from
        |> Maybe.andThen
            (\( block, maybeItem ) ->
                case block of
                    Data.Block.FloorBlock _ ->
                        if args.improvements |> List.member Data.Improvement.MinecartCanCollect then
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
                                                |> Data.World.removeItem args.from
                                        )
                                        (\s ->
                                            s
                                                |> Data.Effect.PlaySound
                                                |> List.singleton
                                        )
                                    )

                        else
                            Nothing

                    _ ->
                        world
                            |> Data.World.transfer { from = args.from, to = args.to }
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
                case Data.World.getBlock p world of
                    Just (Data.Block.FloorBlock Data.Floor.Track) ->
                        True

                    Just (Data.Block.FloorBlock Data.Floor.RailwayTrack) ->
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
