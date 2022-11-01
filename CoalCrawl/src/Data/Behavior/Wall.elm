module Data.Behavior.Wall exposing (..)

import Data.Block
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.World
import Random exposing (Generator)


mine : ( Int, Int ) -> Game -> Generator Game
mine ( x, y ) game =
    game.world
        |> Data.World.get ( x, y )
        |> Maybe.andThen
            (\block ->
                case block of
                    Data.Block.EntityBlock entity ->
                        case entity of
                            Data.Entity.Vein item ->
                                Just (Just item)

                            Data.Entity.Wall ->
                                Just Nothing

                            _ ->
                                Nothing

                    Data.Block.FloorBlock _ ->
                        Nothing
            )
        |> Maybe.map
            (\maybeItem ->
                game.world
                    |> Data.World.removeEntity ( x, y )
                    |> Data.World.insertFloor ( x, y ) (Data.Floor.Ground maybeItem)
                    |> Data.World.update ( x, y - 1 )
                        (\maybe ->
                            maybe
                                |> Maybe.withDefault (Data.Block.EntityBlock Data.Entity.Wall)
                                |> Just
                        )
                    |> (\dict ->
                            [ ( 2, ( x, y + 1 ) )
                            , ( 1, ( x - 1, y ) )
                            , ( 1, ( x + 1, y ) )
                            ]
                                |> List.foldl
                                    (\( prob, pos ) ->
                                        Random.andThen
                                            (\d ->
                                                Random.map2
                                                    (\int item ->
                                                        d
                                                            |> Data.World.update pos
                                                                (\maybe ->
                                                                    maybe
                                                                        |> Maybe.withDefault
                                                                            ((if int /= 0 then
                                                                                Data.Entity.Vein item

                                                                              else
                                                                                Data.Entity.Wall
                                                                             )
                                                                                |> Data.Block.EntityBlock
                                                                            )
                                                                        |> Just
                                                                )
                                                    )
                                                    (Random.int 0 prob)
                                                    (Random.uniform Data.Item.Coal [ Data.Item.IronOre ])
                                            )
                                    )
                                    (Random.constant dict)
                       )
                    |> Random.map
                        (\world ->
                            { game | world = world }
                        )
            )
        |> Maybe.withDefault (Random.constant game)
