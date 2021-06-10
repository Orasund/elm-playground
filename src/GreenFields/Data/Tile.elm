module GreenFields.Data.Tile exposing (Tile, generate)

import GreenFields.Data.Building as Building exposing (Building)
import Random exposing (Generator)
import Time exposing (Posix)


type alias Tile =
    { building : Building
    , timestamp : Posix
    }


generate : ( Int, Int ) -> Tile
generate ( x, y ) =
    let
        seed =
            (toFloat x + toFloat y * pi)
                * 10000
                |> round
                |> Random.initialSeed
    in
    Random.step
        (Building.generate
            |> Random.map
                (\building ->
                    { building = building
                    , timestamp = Time.millisToPosix 0
                    }
                )
        )
        seed
        |> Tuple.first
