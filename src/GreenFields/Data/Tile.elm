module GreenFields.Data.Tile exposing (Tile, codec, generate, isOld)

import Firestore.Codec as Codec exposing (Codec, Document)
import GreenFields.Data.Building as Building exposing (Building)
import Random
import Time exposing (Posix)
import Time.Extra as Time


type alias Tile =
    { building : Building
    , timestamp : Posix
    , x : Int
    , y : Int
    }


codec : Codec Tile
codec =
    Codec.document Tile
        |> Codec.required "building"
            .building
            (Codec.string
                |> Codec.map Building.fromString Building.toString
            )
        |> Codec.required "timestamp" .timestamp Codec.timestamp
        |> Codec.required "x" .x Codec.int
        |> Codec.required "y" .y Codec.int
        |> Codec.build


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
                    , x = x
                    , y = y
                    }
                )
        )
        seed
        |> Tuple.first


isOld : Posix -> Tile -> Bool
isOld timestamp tile =
    tile.timestamp
        |> Time.addMinutes 5
        |> Time.compare timestamp
        |> (==) GT
