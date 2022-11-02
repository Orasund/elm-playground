module Data.Game exposing (..)

import Config
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
                        , Data.Block.EntityBlock Data.Entity.RailwayTrack
                        )
                    )

        coals =
            [ ( Config.width // 2, 4 )
            , ( Config.width // 2 - 1, 3 )
            , ( Config.width // 2 + 1, 3 )
            ]
    in
    { world =
        [ ( train, Data.Block.FloorBlock Data.Floor.Train )
        , ( player, Data.Block.FloorBlock (Data.Floor.Ground Nothing) )
        ]
            ++ tracks
            ++ (coals |> List.map (\pos -> ( pos, Data.Entity.Vein Data.Item.Coal |> Data.Block.EntityBlock )))
            |> Data.World.fromList
    , player = player |> Data.Player.fromPos
    , train = train |> Data.Train.fromPos
    , selected = player
    }
