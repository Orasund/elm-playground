module Cell exposing (..)

import Color
import Dict exposing (Dict)
import Level exposing (Level)
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


cell1ToColor : { level : Level } -> Maybe Bool -> Cell -> String
cell1ToColor args isActive cell =
    case cell of
        ConnectionCell sort ->
            case isActive of
                Just True ->
                    Color.laserColor args.level

                Just False ->
                    Color.inactiveLaser args.level

                Nothing ->
                    case sort.sendsTo |> Dict.toList of
                        [] ->
                            Color.inactiveLaser args.level

                        _ ->
                            Color.laserColor args.level

        Wall ->
            Color.wallColor

        Origin ->
            case isActive of
                Just True ->
                    Color.laserColor args.level

                Just False ->
                    Color.inactiveLaser args.level

                Nothing ->
                    Color.laserColor args.level

        Target Nothing ->
            case isActive of
                Just True ->
                    Color.laserColor args.level

                Just False ->
                    Color.inactiveLaser args.level

                Nothing ->
                    Color.inactiveLaser args.level

        Target _ ->
            case isActive of
                Just True ->
                    Color.laserColor args.level

                Just False ->
                    Color.inactiveLaser args.level

                Nothing ->
                    Color.laserColor args.level
