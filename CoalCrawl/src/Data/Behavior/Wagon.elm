module Data.Behavior.Wagon exposing (..)

import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Floor
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Train
import Data.Wagon exposing (Wagon)
import Data.World
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
                moveOnTrack args id game

             else
                (moveOnGround args ( pos, wagon ) game
                    |> (\p ->
                            ( { game
                                | world =
                                    game.world
                                        |> Data.World.moveActorTo p id
                              }
                            , p
                            )
                       )
                )
                    |> Random.constant
                    |> Just
            )
                |> Maybe.map (Random.map (\( g, _ ) -> g |> unload id))
                |> Maybe.withDefault (Random.constant game)

        _ ->
            Random.constant game


moveOnGround : { backPos : ( Int, Int ), forwardPos : ( Int, Int ) } -> ( ( Int, Int ), Wagon ) -> Game -> ( Int, Int )
moveOnGround args ( pos, wagon ) game =
    case Data.World.get args.forwardPos game.world of
        Just (Data.Block.FloorBlock _) ->
            args.forwardPos

        _ ->
            case Data.World.get args.backPos game.world of
                Just (Data.Block.FloorBlock _) ->
                    args.backPos

                _ ->
                    pos


moveOnTrack :
    { backPos : ( Int, Int ), forwardPos : ( Int, Int ) }
    -> Int
    -> Game
    -> Maybe (Generator ( Game, ( Int, Int ) ))
moveOnTrack args id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            case
                pos
                    |> Data.Position.neighbors
                    |> List.filter
                        (\p ->
                            (p /= args.backPos)
                                && (Data.World.get p game.world == Just (Data.Block.FloorBlock Data.Floor.Track))
                        )
            of
                head :: tail ->
                    Random.uniform head tail
                        |> Random.map
                            (\p ->
                                game.world
                                    |> Data.World.updateActor id
                                        (\_ ->
                                            wagon
                                                |> Data.Wagon.moveFrom pos
                                                |> Data.Actor.Wagon
                                        )
                                    |> Data.World.moveActorTo p id
                                    |> (\world ->
                                            ( { game
                                                | world = world
                                                , player =
                                                    game.player
                                                        |> Data.Player.startRiding pos
                                                        |> Data.Player.moveTo pos
                                              }
                                            , p
                                            )
                                       )
                            )
                        |> Just

                [] ->
                    game
                        |> moveOnGround args ( pos, wagon )
                        |> (\p ->
                                ( { game
                                    | world =
                                        game.world
                                            |> Data.World.updateActor id (\_ -> Data.Actor.Wagon (wagon |> Data.Wagon.stop))
                                            |> Data.World.moveActorTo p id
                                    , player =
                                        game.player
                                            |> Data.Player.stopRiding
                                            |> Data.Player.moveTo p
                                  }
                                , p
                                )
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
