module Action exposing (..)

import Fish.Common exposing (BreedId)
import Random exposing (Generator)


type Action
    = NewBreed BreedId


randMap : (a -> Generator b) -> Generator ( a, List Action ) -> Generator ( b, List Action )
randMap fun =
    Random.andThen (\( a, l ) -> fun a |> Random.map (\b -> ( b, l )))


randAndThen : (a -> Generator ( b, List Action )) -> Generator ( a, List Action ) -> Generator ( b, List Action )
randAndThen fun =
    Random.andThen
        (\( a, l ) -> fun a |> Random.map (Tuple.mapSecond (\l2 -> l2 ++ l)))
