module Data.Game exposing (..)

import Config
import Data.Block exposing (Block(..))
import Data.Player exposing (Player)
import Data.Train exposing (Train)
import Dict exposing (Dict)
import Random exposing (Generator)


type alias Game =
    { world : Dict ( Int, Int ) Block
    , player : Player
    , train : Train
    , selected : ( Int, Int )
    }


select : ( Int, Int ) -> Game -> Game
select pos game =
    { game | selected = pos }


new : Game
new =
    let
        train =
            ( Config.width // 2, 2 )

        player =
            ( Config.width // 2, 3 )

        tracks =
            List.range 0 1
                |> List.map (\i -> ( ( Config.width // 2, i ), Data.Block.Track ))

        coals =
            [ ( Config.width // 2, 4 )
            , ( Config.width // 2 - 1, 3 )
            , ( Config.width // 2 + 1, 3 )
            ]
    in
    { world =
        [ ( train, Data.Block.Train )
        , ( player, Data.Block.Ground Nothing )
        ]
            ++ tracks
            ++ (coals |> List.map (\pos -> ( pos, Data.Block.CoalVein )))
            |> Dict.fromList
    , player = player |> Data.Player.fromPos
    , train = train |> Data.Train.fromPos
    , selected = player
    }
