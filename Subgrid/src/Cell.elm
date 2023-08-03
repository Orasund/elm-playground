module Cell exposing (..)

import Color
import Dict exposing (Dict)
import Level exposing (Level)
import RelativePos exposing (RelativePos)


type alias Connection =
    { moduleId : Int
    , rotation : Int
    , sendsTo : Dict RelativePos { from : RelativePos, originId : Int }
    }


type Cell
    = ConnectionCell Connection
    | Wall
    | Origin { id : Int }
    | Target
        { sendsTo : Dict RelativePos { originId : Int }
        , id : Int
        }


connectionLevel1 : Dict RelativePos { from : RelativePos, originId : Int } -> Connection
connectionLevel1 sendsTo =
    { moduleId = -1
    , rotation = 0
    , sendsTo = sendsTo
    }


toColor : { level : Level, amount : Int, connectedPathIds : List Int } -> Maybe Bool -> Cell -> String
toColor args isActive cell =
    case cell of
        ConnectionCell sort ->
            case isActive of
                Just True ->
                    Color.laserColor args.level args.amount

                Just False ->
                    Color.inactiveLaser args.level

                Nothing ->
                    case sort.sendsTo |> Dict.toList of
                        [] ->
                            Color.inactiveLaser args.level

                        _ ->
                            Color.laserColor args.level args.amount

        Wall ->
            Color.wallColor

        Origin _ ->
            case isActive of
                Just True ->
                    Color.laserColor args.level args.amount

                Just False ->
                    Color.inactiveLaser args.level

                Nothing ->
                    Color.laserColor args.level args.amount

        Target { sendsTo } ->
            case isActive of
                Just True ->
                    Color.laserColor args.level args.amount

                Just False ->
                    Color.inactiveLaser args.level

                Nothing ->
                    if sendsTo == Dict.empty then
                        Color.inactiveLaser args.level

                    else
                        Color.laserColor args.level args.amount
