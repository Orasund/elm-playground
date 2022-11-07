module Data.Effect exposing (..)

import Data.Sound exposing (Sound)
import Random exposing (Generator)


type Effect
    = PlaySound Sound
    | OpenModal
    | ShowPromt String


andThen : (a -> Generator ( b, List Effect )) -> Generator ( a, List Effect ) -> Generator ( b, List Effect )
andThen fun =
    Random.andThen
        (\( a, l ) ->
            fun a |> Random.map (Tuple.mapSecond ((++) l))
        )
