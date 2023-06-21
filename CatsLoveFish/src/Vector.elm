module Vector exposing (..)


to : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
to ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


length : ( Float, Float ) -> Float
length ( x, y ) =
    x * x + y * y |> sqrt


angle : ( Float, Float ) -> Float
angle pos =
    pos |> toPolar |> (\( _, a ) -> a)


scaleBy : Float -> ( Float, Float ) -> ( Float, Float )
scaleBy amount =
    Tuple.mapBoth ((*) amount) ((*) amount)


normalize : ( Float, Float ) -> ( Float, Float )
normalize v =
    v |> scaleBy (1 / length v)


addTo : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addTo ( x, y ) =
    Tuple.mapBoth ((+) x) ((+) y)
