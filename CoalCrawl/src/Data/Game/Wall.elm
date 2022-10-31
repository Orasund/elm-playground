module Data.Game.Wall exposing (..)

import Data.Block
import Data.Game exposing (Game)
import Data.Item
import Dict
import Random exposing (Generator)


mine : ( Int, Int ) -> Game -> Generator Game
mine ( x, y ) game =
    game.world
        |> Dict.get ( x, y )
        |> Maybe.andThen
            (\block ->
                case block of
                    Data.Block.CoalVein ->
                        Just (Just Data.Item.Coal)

                    Data.Block.Wall ->
                        Just Nothing

                    _ ->
                        Nothing
            )
        |> Maybe.map
            (\maybeItem ->
                game.world
                    |> Dict.insert ( x, y ) (Data.Block.Ground maybeItem)
                    |> Dict.update ( x, y - 1 )
                        (\maybe ->
                            maybe |> Maybe.withDefault Data.Block.Wall |> Just
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
                                                Random.int 0 prob
                                                    |> Random.map
                                                        (\int ->
                                                            d
                                                                |> Dict.update pos
                                                                    (\maybe ->
                                                                        maybe
                                                                            |> Maybe.withDefault
                                                                                (if int /= 0 then
                                                                                    Data.Block.CoalVein

                                                                                 else
                                                                                    Data.Block.Wall
                                                                                )
                                                                            |> Just
                                                                    )
                                                        )
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
