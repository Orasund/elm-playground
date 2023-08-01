module RelativePos exposing (..)

import Config
import Dict exposing (Dict)
import Dir exposing (Dir)
import Level exposing (Level)


type alias RelativePos =
    ( ( Int, Int ), String )


list : Level -> List RelativePos
list level =
    List.range 0 3
        |> List.concatMap (\i -> [ ( -1, i ), ( Config.maxPos level, i ), ( i, -1 ), ( i, Config.maxPos level ) ])
        |> List.map fromTuple


fromTuple : ( Int, Int ) -> RelativePos
fromTuple pos =
    ( pos, "RelativePos" )


rotate : Level -> Int -> RelativePos -> RelativePos
rotate level amount a =
    List.range 0 (amount - 1)
        |> List.foldl (\_ -> rotateClockwise level) a


rotationMatrix : Level -> Dict RelativePos RelativePos
rotationMatrix level =
    List.range 0 (Config.maxPos level - 1)
        |> List.map
            (\i ->
                [ ( ( -1, i ), ( Config.maxPos level - 1 - i, -1 ) )
                , ( ( i, -1 ), ( Config.maxPos level, i ) )
                , ( ( Config.maxPos level, Config.maxPos level - 1 - i ), ( i, Config.maxPos level ) )
                , ( ( i, Config.maxPos level ), ( -1, i ) )
                ]
            )
        |> List.concat
        |> List.map (Tuple.mapBoth fromTuple fromTuple)
        |> Dict.fromList


rotateClockwise : Level -> RelativePos -> RelativePos
rotateClockwise level relPos =
    case rotationMatrix level |> Dict.get relPos of
        Just pos ->
            pos

        Nothing ->
            Debug.todo "tried rotating a center position"


reverse : Level -> RelativePos -> RelativePos
reverse level ( ( x, y ), _ ) =
    let
        rev i =
            if i == -1 then
                Config.maxPos level

            else if i == Config.maxPos level then
                -1

            else
                i
    in
    ( rev x, rev y )
        |> fromTuple


toDir : Level -> RelativePos -> Dir
toDir level ( ( x, y ), _ ) =
    let
        minPos =
            -1

        maxPos =
            Config.maxPos level
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
        Debug.todo ("trying to convert " ++ String.fromInt x ++ "," ++ String.fromInt y ++ "with maxPos " ++ String.fromInt maxPos)


unsafeToTuple : RelativePos -> ( Int, Int )
unsafeToTuple ( ( x, y ), _ ) =
    ( x, y )
