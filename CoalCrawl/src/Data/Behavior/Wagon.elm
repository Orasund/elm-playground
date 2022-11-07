module Data.Behavior.Wagon exposing (..)

import AnyBag
import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Floor
import Data.Game exposing (Game)
import Data.Position
import Data.Sound exposing (Sound)
import Data.Train
import Data.Wagon exposing (Wagon)
import Data.World exposing (World)
import Dict
import Random exposing (Generator)


act :
    { backPos : ( Int, Int ) }
    -> Int
    -> Game
    -> Generator ( Game, List Sound )
act args id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            if Data.World.getFloor pos game.world == Just Data.Floor.Track then
                game
                    |> move args id ( pos, wagon )

            else
                Random.constant ( game, [] )

        _ ->
            Random.constant ( game, [] )


move : { backPos : ( Int, Int ) } -> Int -> ( ( Int, Int ), Wagon ) -> Game -> Generator ( Game, List Sound )
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
            |> Random.constant
    )
        |> Random.map
            (\p ->
                game.world
                    |> Data.World.moveActorTo p id
                    |> Data.World.updateActor id
                        (\_ -> Data.Actor.Wagon (wagon |> Data.Wagon.moveFrom pos))
            )
        |> Random.map (\world -> { game | world = world })
        |> Random.map (unload id)


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
    -> ( ( Int, Int ), Wagon )
    -> World
    -> Generator ( Int, Int )
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
        head :: tail ->
            Random.uniform head tail

        [] ->
            world
                |> moveOnGround args ( pos, wagon )
                |> Random.constant


unload : Int -> Game -> ( Game, List Sound )
unload id game =
    case game.world.actors |> Dict.get id of
        Just ( pos, Data.Actor.Wagon wagon ) ->
            if List.member game.train.pos (Data.Position.neighbors pos) && (AnyBag.size wagon.items > 0) then
                ( { game
                    | train = Data.Train.addAll wagon.items game.train
                    , world =
                        game.world
                            |> Data.World.updateActor id (\_ -> Data.Actor.Wagon (Data.Wagon.unload wagon))
                  }
                , [ Data.Sound.Unload ]
                )

            else
                ( game, [] )

        _ ->
            ( game, [] )
