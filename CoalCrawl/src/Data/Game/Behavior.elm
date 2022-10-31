module Data.Game.Behavior exposing (..)

import Data.Game exposing (Game)
import Data.Game.Player
import Data.Game.Train
import Random exposing (Generator)


passTime : Game -> Generator Game
passTime game =
    game
        |> Data.Game.Player.passTime
        |> Random.andThen Data.Game.Train.passTime
