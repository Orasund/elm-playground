module Cell exposing (..)

import Color
import Dict exposing (Dict)
import RelativePos exposing (RelativePos)


type alias Connection =
    { moduleId : Int
    , rotation : Int
    , sendsTo : Dict RelativePos { from : RelativePos }
    }


type Cell
    = ConnectionCell Connection
    | Wall
    | Origin
    | Target (Maybe ( Int, Int ))


connectionLevel1 : Dict RelativePos { from : RelativePos } -> Connection
connectionLevel1 sendsTo =
    { moduleId = -1
    , rotation = 0
    , sendsTo = sendsTo
    }


cell1ToColor : { laserColor : String } -> Maybe Bool -> Cell -> String
cell1ToColor args isActive cell1 =
    case cell1 of
        ConnectionCell sort ->
            case isActive of
                Just True ->
                    args.laserColor

                Just False ->
                    Color.inactiveLaser

                Nothing ->
                    case sort.sendsTo |> Dict.toList of
                        [] ->
                            Color.inactiveLaser

                        _ ->
                            args.laserColor

        Wall ->
            Color.wallColor

        Origin ->
            case isActive of
                Just True ->
                    args.laserColor

                Just False ->
                    Color.inactiveLaser

                Nothing ->
                    args.laserColor

        Target Nothing ->
            case isActive of
                Just True ->
                    args.laserColor

                Just False ->
                    Color.inactiveLaser

                Nothing ->
                    Color.inactiveLaser

        Target _ ->
            case isActive of
                Just True ->
                    args.laserColor

                Just False ->
                    Color.inactiveLaser

                Nothing ->
                    args.laserColor
