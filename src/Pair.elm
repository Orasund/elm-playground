module Pair exposing (Pair, foldl, foldr, map, map2)


type alias Pair a =
    ( a, a )


map : (a -> b) -> Pair a -> Pair b
map fun ( a, b ) =
    ( fun a, fun b )


map2 : (a -> b -> c) -> Pair a -> Pair b -> Pair c
map2 fun ( a, b ) ( c, d ) =
    ( fun a c, fun b d )


foldl : (a -> b -> b) -> b -> Pair a -> b
foldl fun c ( a, b ) =
    fun b <| fun a c


foldr : (a -> b -> b) -> b -> Pair a -> b
foldr fun c ( a, b ) =
    fun a <| fun b c
