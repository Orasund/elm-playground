module Data.Behavior exposing (..)

import Data.Behavior.Player
import Data.Behavior.Train
import Data.Game exposing (Game)
import Random exposing (Generator)


passTime : Game -> Generator Game
passTime game =
    game
        |> Data.Behavior.Player.passTime
        |> Random.andThen Data.Behavior.Train.passTime
