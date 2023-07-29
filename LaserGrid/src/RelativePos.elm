module RelativePos exposing (..)

import Dict exposing (Dict)
import Dir exposing (Dir)
import Svg.Attributes exposing (from)


type alias RelativePos =
    ( ( Int, Int ), String )


list =
    List.range 0 3
        |> List.concatMap (\i -> [ ( -1, i ), ( 4, i ), ( i, -1 ), ( i, 4 ) ])
        |> List.map fromTuple


fromTuple : ( Int, Int ) -> RelativePos
fromTuple pos =
    ( pos, "RelativePos" )


rotate : Int -> RelativePos -> RelativePos
rotate amount a =
    List.range 0 (amount - 1)
        |> List.foldl (\_ -> rotateClockwise) a


rotationMatrix : Dict RelativePos RelativePos
rotationMatrix =
    [ ( ( -1, 0 ), ( 3, -1 ) )
    , ( ( -1, 1 ), ( 2, -1 ) )
    , ( ( -1, 2 ), ( 1, -1 ) )
    , ( ( -1, 3 ), ( 0, -1 ) )
    , ( ( 3, -1 ), ( 4, 3 ) )
    , ( ( 2, -1 ), ( 4, 2 ) )
    , ( ( 1, -1 ), ( 4, 1 ) )
    , ( ( 0, -1 ), ( 4, 0 ) )
    , ( ( 4, 3 ), ( 0, 4 ) )
    , ( ( 4, 2 ), ( 1, 4 ) )
    , ( ( 4, 1 ), ( 2, 4 ) )
    , ( ( 4, 0 ), ( 3, 4 ) )
    , ( ( 0, 4 ), ( -1, 0 ) )
    , ( ( 1, 4 ), ( -1, 1 ) )
    , ( ( 2, 4 ), ( -1, 2 ) )
    , ( ( 3, 4 ), ( -1, 3 ) )
    ]
        |> List.map (Tuple.mapBoth fromTuple fromTuple)
        |> Dict.fromList


{-|

    (0,-1) ->

-}
rotateClockwise : RelativePos -> RelativePos
rotateClockwise relPos =
    case rotationMatrix |> Dict.get relPos of
        Just pos ->
            pos

        Nothing ->
            Debug.todo "tried rotating a center position"


reverse : RelativePos -> RelativePos
reverse ( ( x, y ), _ ) =
    let
        rev i =
            if i == -1 then
                4

            else if i == 4 then
                -1

            else
                i
    in
    ( rev x, rev y )
        |> fromTuple


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
