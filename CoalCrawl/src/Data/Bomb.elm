module Data.Bomb exposing (..)

import Config


type alias Bomb =
    { explodesIn : Int }


tick : Bomb -> Maybe Bomb
tick bomb =
    if bomb.explodesIn > 1 then
        { explodesIn = bomb.explodesIn - 1 }
            |> Just

    else
        Nothing


new : Bomb
new =
    { explodesIn = Config.bombExplosionTime }
