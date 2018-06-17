module Roguelike.Game exposing (Game, applyDirection)

import Dict exposing (Dict)
import Roguelike.Cell as Cell exposing (Cell(..), ConsumableType(..), Direction(..), MiscellaneousType(..), EffectType(..), EnemyType(..), Item(..), SolidType(..))
import Roguelike.Map as Map exposing (Map)
import Roguelike.Player as Player exposing (PlayerData)


type alias Game =
    ( PlayerData, Map Cell )


applyDirection : Int -> Direction -> Game -> Game
applyDirection size dir ( playerData, map ) =
    let
        player : Maybe ( Map.Location, Direction )
        player =
            Player.getCell map
    in
    case player of
        Just ( ( x, y ), d ) ->
            if d == dir then
                ( playerData, map )
                    |> Player.move size ( x, y ) d
                    |> (\tuple ->
                            tuple
                                |> Tuple.second
                                |> Dict.foldl
                                    (\( i, j ) cell out ->
                                        case cell of
                                            Enemy b ->
                                                out |> updateEnemy ( i, j ) b

                                            Effect a ->
                                                out
                                                    |> Tuple.mapSecond (Dict.remove ( i, j ))

                                            _ ->
                                                out
                                    )
                                    tuple
                       )
            else
                ( playerData
                , map |> Player.face ( x, y ) dir
                )

        Nothing ->
            ( playerData, map )


removeCell : Map.Location -> EnemyType -> Map Cell -> Map Cell
removeCell location enemyType map =
    map |> Map.remove location


updateEnemy : Map.Location -> EnemyType -> Game -> Game
updateEnemy ( i, j ) enemyType ( playerData, map ) =
    let
        player : Maybe ( Map.Location, Direction )
        player =
            Player.getCell map
    in
    (case player of
        Just ( ( x, y ), _ ) ->
            [ Up, Down, Left, Right ]
                |> List.filter (\dir -> ( i - x, j - y ) == Map.dirCoordinates dir)
                |> (\list ->
                        case list of
                            _ :: _ ->
                                ( playerData |> Player.attack
                                , map
                                )

                            _ ->
                                ( playerData, map )
                   )

        Nothing ->
            ( playerData, map )
    )
        |> (\tuple ->
                case enemyType of
                    PlacedBombe ->
                        [ Up, Down, Left, Right ]
                            |> List.foldl
                                (\dir t ->
                                    let
                                        ( x, y ) =
                                            Map.dirCoordinates dir
                                    in
                                    case t |> Tuple.second |> Dict.get ( i + x, j + y ) of
                                        Just (Enemy _) ->
                                            t
                                                |> Tuple.mapSecond
                                                    (Dict.update ( i + x, j + y ) (always (Just (Item (Miscellaneous Bone)))))

                                        Nothing ->
                                            t
                                                |> Tuple.mapSecond
                                                    (Dict.update ( i + x, j + y ) (always (Just (Effect Smoke))))

                                        _ ->
                                            t
                                )
                                tuple
                            |> Tuple.mapSecond
                                (Dict.update ( i, j ) (always (Just (Effect Smoke))))

                    _ ->
                        tuple
           )
