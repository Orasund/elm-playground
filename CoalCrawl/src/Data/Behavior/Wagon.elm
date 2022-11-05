module Data.Behavior.Wagon exposing (..)

import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Floor
import Data.Game exposing (Game)
import Data.Position
import Data.Train
import Data.Wagon exposing (Wagon)
import Data.World exposing (World)
import Dict
import Random exposing (Generator)


move :
    { backPos : ( Int, Int ) }
    -> Int
    -> Game
    -> Generator Game
move { backPos } id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            let
                forwardPos =
                    backPos
                        |> Data.Position.vecTo pos
                        |> Data.Position.plus pos

                args =
                    { backPos = backPos, forwardPos = forwardPos }
            in
            (if Data.World.getFloor pos game.world == Just Data.Floor.Track then
                game.world
                    |> moveOnTrack args id

             else
                game.world
                    |> moveOnGround args ( pos, wagon )
                    |> (\p -> game.world |> Data.World.moveActorTo p id)
                    |> Random.constant
                    |> Just
            )
                |> Maybe.map (Random.map (\world -> { game | world = world } |> unload id))
                |> Maybe.withDefault (Random.constant game)

        _ ->
            Random.constant game


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Wagon ) -> World -> ( Int, Int )
moveOnGround args ( pos, wagon ) world =
    case Data.World.get args.forwardPos world of
        Just (Data.Block.FloorBlock _) ->
            args.forwardPos

        _ ->
            case Data.World.get args.backPos world of
                Just (Data.Block.FloorBlock _) ->
                    args.backPos

                _ ->
                    pos


moveOnTrack :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> Int
    -> World
    -> Maybe (Generator World)
moveOnTrack args id world =
    case world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            case
                pos
                    |> Data.Position.neighbors
                    |> List.filter
                        (\p ->
                            (p /= args.backPos)
                                && (Data.World.get p world == Just (Data.Block.FloorBlock Data.Floor.Track))
                        )
            of
                head :: tail ->
                    Random.uniform head tail
                        |> Random.map
                            (\p ->
                                world
                                    |> Data.World.updateActor id
                                        (\_ ->
                                            wagon
                                                |> Data.Wagon.moveFrom pos
                                                |> Data.Actor.Wagon
                                        )
                                    |> Data.World.moveActorTo p id
                            )
                        |> Just

                [] ->
                    world
                        |> moveOnGround args ( pos, wagon )
                        |> (\p ->
                                world
                                    |> Data.World.updateActor id (\_ -> Data.Actor.Wagon (wagon |> Data.Wagon.stop))
                                    |> Data.World.moveActorTo p id
                           )
                        |> Random.constant
                        |> Just

        _ ->
            Nothing


unload : Int -> Game -> Game
unload id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            if List.member game.train.pos (Data.Position.neighbors pos) then
                { game
                    | train = Data.Train.addAll wagon.items game.train
                    , world =
                        game.world
                            |> Data.World.updateActor id (\_ -> Data.Actor.Wagon (Data.Wagon.unload wagon))
                }

            else
                game

        _ ->
            game
