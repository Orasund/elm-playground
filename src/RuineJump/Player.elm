module RuineJump.Player exposing (FaceingDirection(..), Player, PlayerAction(..), fall, jump, move)

import Dict exposing (Dict)
import RuineJump.Config as Config


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


fall : Map a -> Player -> Player
fall map ({ pos, action, faceing } as player) =
    let
        ( x, y ) =
            pos
    in
    { player | pos = ( x, y + 1 ), action = Falling }
        |> (if
                case faceing of
                    FaceingLeft ->
                        x <= 0

                    FaceingRight ->
                        x >= Config.width - 2
            then
                identity

            else
                forwardByOne map
           )


upwardsByOne : Map a -> Player -> Player
upwardsByOne map ({ pos, action, faceing } as player) =
    let
        ( x, y ) =
            pos

        defaultCase : Player
        defaultCase =
            player
    in
    case map |> Dict.get ( x, y - 1 ) of
        Nothing ->
            case map |> Dict.get ( x + 1, y - 1 ) of
                Nothing ->
                    { player | pos = ( x, y - 1 ) }

                Just _ ->
                    defaultCase

        Just _ ->
            defaultCase


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
            case map |> Dict.get ( x + dir, y - 1 ) of
                Nothing ->
                    case map |> Dict.get ( x + dir, y - 2 ) of
                        Nothing ->
                            { player | pos = ( newX, y - 1 ) }

                        Just _ ->
                            defaultCase

                Just _ ->
                    defaultCase


jump : Map a -> Player -> Player
jump map ({ pos, action } as player) =
    let
        defaultCase : Player
        defaultCase =
            player
    in
    case action of
        Standing ->
            { player | action = Falling }
                |> upwardsByOne map
                |> upwardsByOne map
                |> upwardsByOne map
                |> upwardsByOne map
                |> upwardsByOne map

        Falling ->
            defaultCase


move : FaceingDirection -> Map a -> Player -> Player
move direction map ({ pos, action } as player) =
    let
        defaultCase : Player -> Player
        defaultCase =
            identity

        x : Int
        x =
            pos |> Tuple.first
    in
    { player | faceing = direction }
        |> (case action of
                Standing ->
                    if
                        case direction of
                            FaceingLeft ->
                                x <= 0

                            FaceingRight ->
                                x >= Config.width - 2
                    then
                        defaultCase

                    else
                        forwardByOne map
                            >> forwardByOne map

                Falling ->
                    defaultCase
           )
