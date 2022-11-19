module Data.Effect exposing (..)

import Data.Sound exposing (Sound)
import Random exposing (Generator)


type Effect
    = PlaySound Sound
    | OpenModal
    | ShowPromt String
    | LevelUp


withNone : a -> Generator ( a, List Effect )
withNone a =
    Random.constant ( a, [] )


genWithNone : Generator a -> Generator ( a, List Effect )
genWithNone =
    Random.map (\a -> ( a, [] ))


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
