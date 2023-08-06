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
        { from : Dict RelativePos { originId : Int }
        , id : Int
        }


connectionLevel1 : Dict RelativePos { from : RelativePos, originId : Int } -> Connection
connectionLevel1 sendsTo =
    { moduleId = -1
    , rotation = 0
    , sendsTo = sendsTo
    }


toColor : { level : Level } -> Maybe { originId : Maybe Int } -> Cell -> String
toColor args isActive cell =
    case cell of
        ConnectionCell sort ->
            case isActive of
                Just { originId } ->
                    originId
                        |> Maybe.map (Color.laserColor args.level)
                        |> Maybe.withDefault (Color.inactiveLaser args.level)

                Nothing ->
                    case sort.sendsTo |> Dict.toList of
                        [] ->
                            Color.inactiveLaser args.level

                        ( _, { originId } ) :: _ ->
                            Color.laserColor args.level originId

        Wall ->
            Color.wallColor

        Origin { id } ->
            case isActive of
                Just { originId } ->
                    originId
                        |> Maybe.map (Color.laserColor args.level)
                        |> Maybe.withDefault (Color.inactiveLaser args.level)

                Nothing ->
                    Color.laserColor args.level id

        Target { from } ->
            case isActive of
                Just { originId } ->
                    originId
                        |> Maybe.map (Color.laserColor args.level)
                        |> Maybe.withDefault (Color.inactiveLaser args.level)

                Nothing ->
                    case from |> Dict.toList of
                        [] ->
                            Color.inactiveLaser args.level

                        ( _, { originId } ) :: _ ->
                            Color.laserColor args.level originId
