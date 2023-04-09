module Internal exposing (..)


minBy : (a -> comparable) -> List a -> Maybe a
minBy fun list =
    list
        |> List.foldl
            (\a maybe ->
                maybe
                    |> Maybe.map
                        (\( minB, b ) ->
                            fun a
                                |> (\minA ->
                                        if minA < minB then
                                            ( minA, a )

                                        else
                                            ( minB, b )
                                   )
                        )
                    |> Maybe.withDefault ( fun a, a )
                    |> Just
            )
            Nothing
        |> Maybe.map Tuple.second
