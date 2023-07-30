module RelativePos exposing (..)

import Dict exposing (Dict)
import Dir exposing (Dir)


type alias RelativePos =
    ( ( Int, Int ), String )


list : { maxPos : Int } -> List RelativePos
list args =
    List.range 0 3
        |> List.concatMap (\i -> [ ( -1, i ), ( args.maxPos, i ), ( i, -1 ), ( i, args.maxPos ) ])
        |> List.map fromTuple


fromTuple : ( Int, Int ) -> RelativePos
fromTuple pos =
    ( pos, "RelativePos" )


rotate : { maxPos : Int } -> Int -> RelativePos -> RelativePos
rotate args amount a =
    List.range 0 (amount - 1)
        |> List.foldl (\_ -> rotateClockwise args) a


rotationMatrix : { maxPos : Int } -> Dict RelativePos RelativePos
rotationMatrix args =
    List.range 0 (args.maxPos - 1)
        |> List.map
            (\i ->
                [ ( ( -1, i ), ( args.maxPos - 1 - i, -1 ) )
                , ( ( i, -1 ), ( args.maxPos, i ) )
                , ( ( args.maxPos, args.maxPos - 1 - i ), ( i, args.maxPos ) )
                , ( ( i, args.maxPos ), ( -1, i ) )
                ]
            )
        |> List.concat
        |> List.map (Tuple.mapBoth fromTuple fromTuple)
        |> Dict.fromList


rotateClockwise : { maxPos : Int } -> RelativePos -> RelativePos
rotateClockwise args relPos =
    case rotationMatrix args |> Dict.get relPos of
        Just pos ->
            pos

        Nothing ->
            Debug.todo "tried rotating a center position"


reverse : { maxPos : Int } -> RelativePos -> RelativePos
reverse args ( ( x, y ), _ ) =
    let
        rev i =
            if i == -1 then
                args.maxPos

            else if i == args.maxPos then
                -1

            else
                i
    in
    ( rev x, rev y )
        |> fromTuple


toDir : { maxPos : Int } -> RelativePos -> Dir
toDir args ( ( x, y ), _ ) =
    let
        minPos =
            -1

        maxPos =
            args.maxPos
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
        Debug.todo ("trying to convert " ++ String.fromInt x ++ "," ++ String.fromInt y)


unsafeToTuple : RelativePos -> ( Int, Int )
unsafeToTuple ( ( x, y ), _ ) =
    ( x, y )
