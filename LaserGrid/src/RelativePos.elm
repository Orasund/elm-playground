module RelativePos exposing (..)


type alias RelativePos =
    ( ( Int, Int ), String )


fromTuple : ( Int, Int ) -> RelativePos
fromTuple pos =
    ( pos, "RelativePos" )


add : ( Int, Int ) -> RelativePos -> ( Int, Int )
add ( x1, y1 ) ( ( x2, y2 ), _ ) =
    ( x1 + x2, y1 + y2 )


rotate : Int -> RelativePos -> RelativePos
rotate amount a =
    List.range 0 (amount - 1)
        |> List.foldl (\_ -> rotateClockwise) a


rotateClockwise : RelativePos -> RelativePos
rotateClockwise ( ( x, y ), _ ) =
    let
        maxInt =
            4

        minInt =
            -1
    in
    (if x == minInt then
        ( y, maxInt )

     else if y == maxInt then
        ( maxInt, x )

     else if x == maxInt then
        ( y, minInt )

     else
        ( minInt, x )
    )
        |> fromTuple
