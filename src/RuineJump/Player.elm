module RuineJump.Player exposing (FaceingDirection(..), Player, PlayerAction(..), fall, move)

import Dict exposing (Dict)


type alias Map a =
    Dict ( Int, Int ) a


type alias Player =
    { pos : ( Int, Int ), action : PlayerAction, faceing : FaceingDirection }


type FaceingDirection
    = FaceingLeft
    | FaceingRight


type PlayerAction
    = Standing
    | Falling


fall : Player -> Player
fall ({ pos, action } as player) =
    let
        ( x, y ) =
            pos
    in
    { player | pos = ( x, y + 1 ), action = Falling }


forwardByOne : Map a -> Player -> Player
forwardByOne map ({ pos, action, faceing } as player) =
    let
        ( x, y ) =
            pos

        dir : Int
        dir =
            case faceing of
                FaceingLeft ->
                    -1

                FaceingRight ->
                    2

        newX : Int
        newX =
            case faceing of
                FaceingLeft ->
                    x - 1

                FaceingRight ->
                    x + 1

        defaultCase : Player
        defaultCase =
            player
    in
    case map |> Dict.get ( x + dir, y ) of
        Nothing ->
            case map |> Dict.get ( x + dir, y + 1 ) of
                Nothing ->
                    { player | pos = ( newX, y ) }

                Just _ ->
                    case map |> Dict.get ( x + dir, y - 1 ) of
                        Nothing ->
                            { player | pos = ( newX, y - 1 ) }

                        Just _ ->
                            defaultCase

        Just _ ->
            defaultCase


move : FaceingDirection -> Map a -> Player -> Player
move direction map ({ pos, action } as player) =
    let
        defaultCase : Player
        defaultCase =
            player
    in
    case action of
        Standing ->
            { player | faceing = direction }
                |> forwardByOne map
                |> forwardByOne map

        _ ->
            defaultCase
