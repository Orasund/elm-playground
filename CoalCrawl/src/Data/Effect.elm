module Data.Effect exposing (..)

import Data.Sound exposing (Sound)
import Random exposing (Generator)


type Effect
    = PlaySound Sound
    | OpenModal
    | ShowPromt String
    | LevelUp


andThen : (a -> Generator ( b, List Effect )) -> Generator ( a, List Effect ) -> Generator ( b, List Effect )
andThen fun =
    Random.andThen
        (\( a, l ) ->
            fun a |> Random.map (Tuple.mapSecond ((++) l))
        )


map : (a -> ( b, List Effect )) -> Generator ( a, List Effect ) -> Generator ( b, List Effect )
map fun =
    Random.map
        (\( a, l ) ->
            fun a |> Tuple.mapSecond ((++) l)
        )
