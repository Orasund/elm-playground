module RelativePos exposing (..)

import Dir exposing (Dir)


type alias RelativePos =
    ( ( Int, Int ), String )


fromTuple : ( Int, Int ) -> RelativePos
fromTuple pos =
    ( pos, "RelativePos" )


rotate : Int -> RelativePos -> RelativePos
rotate amount a =
    List.range 0 (amount - 1)
        |> List.foldl (\_ -> rotateClockwise) a


rotateClockwise : RelativePos -> RelativePos
rotateClockwise ( ( x, y ), _ ) =
    ( 4 - (y + 1), x )
        |> fromTuple


reverse : RelativePos -> RelativePos
reverse =
    rotate 2


toDir : RelativePos -> Dir
toDir ( ( x, y ), _ ) =
    let
        minPos =
            -1

        maxPos =
            4
    in
    if x == maxPos then
        Dir.new 0

    else if y == maxPos then
        Dir.new 1

    else if x == minPos then
        Dir.new 2

    else if y == minPos then
        Dir.new 3

    else
        Debug.todo ("trying to convert" ++ String.fromInt x ++ "," ++ String.fromInt y)
