module Data.Behavior.Wall exposing (..)

import Data.Block
import Data.Entity exposing (Entity)
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.World exposing (World)
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

                            Data.Entity.Wall _ ->
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
                    |> generateContent
                        { probability =
                            [ ( 0, ( x, y - 1 ) )
                            , ( 0.8, ( x, y + 1 ) )
                            , ( 0.5, ( x - 1, y ) )
                            , ( 0.5, ( x + 1, y ) )
                            ]
                        , content =
                            Random.weighted ( 1, Data.Entity.Vein Data.Item.Coal )
                                [ ( 1 / 2, Data.Entity.Vein Data.Item.Iron )
                                , ( 1 / 8, Data.Entity.Wall { unstable = True } )
                                ]
                        }
                    |> Random.map (\world -> { game | world = world })
            )
        |> Maybe.withDefault (Random.constant game)


exposedUnstableWall : ( Int, Int ) -> Game -> Generator Game
exposedUnstableWall ( x, y ) game =
    Random.int 0 5
        |> Random.andThen
            (\int ->
                if int == 0 then
                    game.world
                        |> Data.World.insertEntity ( x, y ) Data.Entity.Water
                        |> generateContent
                            { probability =
                                [ ( 0, ( x, y - 1 ) )
                                , ( 0.5, ( x, y + 1 ) )
                                , ( 1, ( x - 1, y ) )
                                , ( 1, ( x + 1, y ) )
                                ]
                            , content =
                                Random.weighted ( 1, Data.Entity.Wall { unstable = True } )
                                    [ ( 1, Data.Entity.Vein Data.Item.Iron )
                                    , ( 1 / 4, Data.Entity.Vein Data.Item.Coal )
                                    , ( 1 / 8, Data.Entity.Vein Data.Item.Gold )
                                    ]
                            }

                else
                    Random.constant game.world
            )
        |> Random.map (\world -> { game | world = world })


generateContent : { probability : List ( Float, ( Int, Int ) ), content : Generator Entity } -> World -> Generator World
generateContent args dict =
    args.probability
        |> List.foldl
            (\( prob, pos ) ->
                Random.andThen
                    (\d ->
                        Random.map2
                            (\float entity ->
                                d
                                    |> Data.World.update pos
                                        (\maybe ->
                                            maybe
                                                |> Maybe.withDefault
                                                    ((if float < prob then
                                                        entity

                                                      else
                                                        Data.Entity.Wall { unstable = False }
                                                     )
                                                        |> Data.Block.EntityBlock
                                                    )
                                                |> Just
                                        )
                            )
                            (Random.float 0 1)
                            args.content
                    )
            )
            (Random.constant dict)
