module Data.Game exposing (..)

import Data.Actor exposing (Actor)
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Improvement exposing (Improvement)
import Data.Item exposing (Item)
import Data.Player exposing (Player)
import Data.Sound
import Data.Train exposing (Train)
import Data.World exposing (World)


type alias Game =
    { world : World
    , player : Player
    , trainId : Int
    , improvements : List Improvement
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
                |> Data.World.setActor game.trainId (Data.Actor.Train train)
    }


setTrainOf : Game -> Train -> Game
setTrainOf game train =
    setTrain train game


addImprovementTo : Game -> Improvement -> Game
addImprovementTo game improvement =
    addImprovement improvement game


addImprovement : Improvement -> Game -> Game
addImprovement improvement game =
    { game | improvements = improvement :: game.improvements }


getImprovements : Game -> List Improvement
getImprovements game =
    game.improvements


setWorldOf : Game -> World -> Game
setWorldOf game world =
    setWorld world game


select : ( Int, Int ) -> Game -> Game
select pos game =
    { game
        | selected = pos
        , player = game.player |> Data.Player.startMovingTo pos
    }


buildFloor : ( Int, Int ) -> ( Item, Int ) -> Floor -> Game -> Maybe ( Game, List Effect )
buildFloor pos ( item, cost ) floor game =
    if
        Data.World.getFloor pos game.world
            == Just Data.Floor.Ground
    then
        game
            |> getTrain
            |> Data.Train.removeItem cost item
            |> Maybe.map
                (\train ->
                    { game
                        | world = game.world |> Data.World.insertFloorAt pos floor
                    }
                        |> setTrain train
                )
            |> Maybe.withDefault game
            |> (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.Build ] ))
            |> Just

    else
        Nothing


buildEntity : ( Int, Int ) -> ( Item, Int ) -> Entity -> Game -> Maybe ( Game, List Effect )
buildEntity pos ( item, cost ) entity game =
    if Data.World.isFloor pos game.world then
        game
            |> getTrain
            |> Data.Train.removeItem cost item
            |> Maybe.map
                (\train ->
                    { game
                        | world = game.world |> Data.World.insertEntityAt pos entity
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
                    Data.Block.ActorBlock ( _, Data.Actor.Minecart minecart ) ->
                        game.world
                            |> Data.World.removeEntity pos
                            |> Data.World.insertAllItems minecart.storage.items pos
                            |> setWorldOf game
                            |> Just

                    Data.Block.EntityBlock (Data.Entity.Container storage) ->
                        game.world
                            |> Data.World.removeEntity pos
                            |> Data.World.insertAllItems storage.items pos
                            |> setWorldOf game
                            |> Just

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

        addTracks w =
            List.range 0 1
                |> List.foldl
                    (\i ->
                        Data.World.insertFloorAt ( 0, i ) Data.Floor.RailwayTrack
                    )
                    w

        addWalls w =
            List.range 0 2
                |> List.concatMap
                    (\y ->
                        [ -1, 1 ]
                            |> List.map (\x -> ( x, y ))
                    )
                |> (::) ( 0, -1 )
                |> List.foldl (Data.World.insertEntity Data.Entity.Wall) w

        addCoals w =
            [ ( 0 - 1, 3 )
            , ( 0 + 1, 3 )
            ]
                |> List.foldl (Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal)) w
    in
    { world =
        Data.World.empty
            |> Data.World.insertFloorAt train Data.Floor.RailwayTrack
            |> Data.World.insertFloorAt player Data.Floor.Ground
            |> addTracks
            |> addWalls
            |> addCoals
            |> Data.World.insertActor
                (train
                    |> Data.Train.fromPos
                    |> Data.Actor.Train
                )
                train
            |> Data.World.insertActor (Data.Actor.Helper Data.Actor.Path) ( 0, 4 )
    , player = player |> Data.Player.fromPos
    , trainId = 0
    , improvements = []
    , selected = player
    }


setWorld : World -> Game -> Game
setWorld world game =
    { game | world = world }
