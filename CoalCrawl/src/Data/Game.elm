module Data.Game exposing (..)

import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Floor
import Data.Item exposing (Item)
import Data.Player exposing (Player)
import Data.Sound
import Data.Train exposing (Train)
import Data.World exposing (World, setActor)


type alias Game =
    { world : World
    , player : Player
    , trainId : Int
    , selected : ( Int, Int )
    }


getTrain : Game -> Train
getTrain game =
    case game.world |> Data.World.getActor game.trainId of
        Just ( _, Data.Actor.Train train ) ->
            train

        _ ->
            Data.Train.fromPos ( 0, 0 )


setTrain : Train -> Game -> Game
setTrain train game =
    { game
        | world =
            game.world
                |> setActor game.trainId (Data.Actor.Train train)
    }


setTrainOf : Game -> Train -> Game
setTrainOf game train =
    setTrain train game


setWorldOf : Game -> World -> Game
setWorldOf game world =
    setWorld world game


select : ( Int, Int ) -> Game -> Game
select pos game =
    { game
        | selected = pos
        , player = game.player |> Data.Player.startMovingTo pos
    }


buildBlock : ( Int, Int ) -> ( Item, Int ) -> Block -> Game -> Maybe ( Game, List Effect )
buildBlock pos ( item, cost ) block game =
    if
        case block of
            FloorBlock _ ->
                Data.World.getFloor pos game.world
                    == Just Data.Floor.Ground

            EntityBlock _ ->
                Data.World.isFloor pos game.world
    then
        game
            |> getTrain
            |> Data.Train.removeItem cost item
            |> Maybe.map
                (\train ->
                    { game
                        | world = game.world |> Data.World.insert pos block
                    }
                        |> setTrain train
                )
            |> Maybe.withDefault game
            |> (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.Build ] ))
            |> Just

    else
        Nothing


buildActor : ( Int, Int ) -> ( Item, Int ) -> Actor -> Game -> Maybe ( Game, List Effect )
buildActor pos ( item, cost ) actor game =
    if Data.World.isFloor pos game.world then
        game
            |> getTrain
            |> Data.Train.removeItem cost item
            |> Maybe.map
                (\train ->
                    { game
                        | world = game.world |> Data.World.insertActorAt pos actor
                    }
                        |> setTrain train
                )
            |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.Build ] ))

    else
        Nothing


destroyBlock : ( Int, Int ) -> Game -> Maybe ( Game, List Effect )
destroyBlock pos game =
    game.world
        |> Data.World.get pos
        |> Maybe.map Tuple.first
        |> Maybe.andThen
            (\block ->
                case block of
                    Data.Block.EntityBlock (Data.Entity.Actor id) ->
                        case game.world |> Data.World.getActor id of
                            Just ( _, Data.Actor.Minecart minecart ) ->
                                game
                                    |> getTrain
                                    |> Data.Train.addAll (List.repeat Config.wagonCost Data.Item.Iron)
                                    |> (\train -> game |> setTrain train)
                                    |> (\g ->
                                            g.world
                                                |> Data.World.removeEntity pos
                                                |> Data.World.insertAllItems minecart.storage.items pos
                                                |> setWorldOf g
                                       )
                                    |> Just

                            Just ( _, Data.Actor.Excavator _ ) ->
                                game.world
                                    |> Data.World.removeEntity pos
                                    |> (\world -> { game | world = world })
                                    |> Just

                            _ ->
                                Nothing

                    Data.Block.FloorBlock Data.Floor.Track ->
                        game.world
                            |> Data.World.removeFloor pos
                            |> (\world -> { game | world = world })
                            |> Just

                    _ ->
                        Nothing
            )
        |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.Destruct ] ))


new : Game
new =
    let
        train =
            ( 0, 2 )

        player =
            ( 0, 3 )

        tracks =
            List.range 0 1
                |> List.map
                    (\i ->
                        ( ( 0, i )
                        , Data.Block.FloorBlock Data.Floor.RailwayTrack
                        )
                    )

        walls =
            List.range 0 1
                |> List.concatMap
                    (\y ->
                        [ -1, 1 ]
                            |> List.map (\x -> ( x, y ))
                    )
                |> (::) ( 0, -1 )
                |> List.map (\p -> ( p, Data.Block.EntityBlock Data.Entity.Wall ))

        coals =
            [ ( 0, 4 )
            , ( 0 - 1, 3 )
            , ( 0 + 1, 3 )
            ]
    in
    { world =
        [ ( train, Data.Block.FloorBlock Data.Floor.RailwayTrack )
        , ( player, Data.Block.FloorBlock Data.Floor.Ground )
        ]
            ++ tracks
            ++ walls
            ++ (coals |> List.map (\pos -> ( pos, Data.Entity.Vein Data.Item.Coal |> Data.Block.EntityBlock )))
            |> Data.World.fromList
            |> Data.World.insertActor
                (train
                    |> Data.Train.fromPos
                    |> Data.Actor.Train
                )
                train
    , player = player |> Data.Player.fromPos
    , trainId = 0
    , selected = player
    }


setWorld : World -> Game -> Game
setWorld world game =
    { game | world = world }
