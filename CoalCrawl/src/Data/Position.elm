module Data.Position exposing (..)


neighbors : ( Int, Int ) -> List ( Int, Int )
neighbors ( x, y ) =
    [ ( x, y + 1 ), ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ) ]


neighborsWithDistance : Int -> ( Int, Int ) -> List ( Int, Int )
neighborsWithDistance r ( x, y ) =
    List.range (x - r) (x + r)
        |> List.concatMap
            (\x0 ->
                List.range (y - r) (y + r)
                    |> List.map (\y0 -> ( x0, y0 ))
            )


vecTo : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
vecTo p ( x, y ) =
    plus p ( -x, -y )


plus : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
plus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )
