module Data.Game exposing (..)

import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block(..))
import Data.Entity
import Data.Floor
import Data.Item
import Data.Player exposing (Player)
import Data.Train exposing (Train)
import Data.World exposing (World)


type alias Game =
    { world : World
    , player : Player
    , train : Train
    , selected : ( Int, Int )
    }


select : ( Int, Int ) -> Game -> Game
select pos game =
    { game | selected = pos }


buildBlock : Int -> Block -> Game -> Game
buildBlock cost block game =
    game.train
        |> Data.Train.removeItem cost Data.Item.Iron
        |> Maybe.map
            (\train ->
                { game
                    | world = game.world |> Data.World.insert game.selected block
                    , train = train
                }
            )
        |> Maybe.withDefault game


buildActor : Int -> Actor -> Game -> Game
buildActor cost actor game =
    game.train
        |> Data.Train.removeItem cost Data.Item.Iron
        |> Maybe.map
            (\train ->
                { game
                    | world = game.world |> Data.World.insertActor game.selected actor
                    , train = train
                }
            )
        |> Maybe.withDefault game


destroyBlock : Game -> Game
destroyBlock game =
    game.world
        |> (case Data.World.get game.selected game.world of
                Just (Data.Block.EntityBlock _) ->
                    Data.World.removeEntity game.selected

                Just (Data.Block.FloorBlock _) ->
                    Data.World.insertFloor game.selected Data.Floor.ground

                Nothing ->
                    identity
           )
        |> (\world -> { game | world = world })


new : Game
new =
    let
        train =
            ( Config.width // 2, 2 )

        player =
            ( Config.width // 2, 3 )

        tracks =
            List.range 0 1
                |> List.map
                    (\i ->
                        ( ( Config.width // 2, i )
                        , Data.Block.FloorBlock Data.Floor.RailwayTrack
                        )
                    )

        coals =
            [ ( Config.width // 2, 4 )
            , ( Config.width // 2 - 1, 3 )
            , ( Config.width // 2 + 1, 3 )
            ]
    in
    { world =
        [ ( train, Data.Block.EntityBlock Data.Entity.Train )
        , ( player, Data.Block.FloorBlock (Data.Floor.Ground Nothing) )
        ]
            ++ tracks
            ++ (coals |> List.map (\pos -> ( pos, Data.Entity.Vein Data.Item.Coal |> Data.Block.EntityBlock )))
            |> Data.World.fromList
    , player = player |> Data.Player.fromPos
    , train = train |> Data.Train.fromPos
    , selected = player
    }
