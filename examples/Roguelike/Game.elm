module Roguelike.Game exposing (Game, applyDirection)

import Dict
import Pair
import Roguelike.Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , Direction(..)
        , EffectType(..)
        , EnemyType(..)
        , Item(..)
        , MiscellaneousType(..)
        , SolidType(..)
        )
import Roguelike.Map as Map exposing (Map)
import Roguelike.Player as Player exposing (PlayerData)


type alias Game =
    ( PlayerData, Map Cell )


applyDirection : Int -> Direction -> Game -> Game
applyDirection size dir ( playerData, map ) =
    case player map of
        Just ( location, d ) ->
            if d == dir then
                ( playerData, map )
                    |> Player.move size location d
                    |> (\tuple ->
                            tuple
                                |> Tuple.second
                                |> Dict.foldl
                                    playerInteration
                                    tuple
                       )
            else
                ( playerData
                , map |> Player.face location dir
                )

        Nothing ->
            ( playerData, map )


playerInteration : Map.Location -> Cell -> Game -> Game
playerInteration location cell game =
    case cell of
        Enemy enemy id ->
            game |> updateEnemy location enemy

        Effect _ ->
            game
                |> Tuple.mapSecond (Dict.remove location)

        _ ->
            game


updateEnemy : Map.Location -> EnemyType -> Game -> Game
updateEnemy location enemyType game =
    game
        |> attackPlayer location
        |> specialBehaviour location enemyType


player : Map Cell -> Maybe ( Map.Location, Direction )
player map =
    Player.getCell map


attackPlayer : Map.Location -> Game -> Game
attackPlayer location ( playerData, map ) =
    case player map of
        Just ( playerLocation, _ ) ->
            [ Up, Down, Left, Right ]
                |> List.filter
                    ((==) (Pair.map2 (-) location playerLocation) << Map.dirCoordinates)
                |> List.head
                |> Maybe.map (always (( playerData, map ) |> Tuple.mapFirst Player.attack))
                |> Maybe.withDefault ( playerData, map )

        Nothing ->
            ( playerData, map )


specialBehaviour : Map.Location -> EnemyType -> Game -> Game
specialBehaviour currentLocation enemyType game =
    case enemyType of
        PlacedBombe ->
            [ Up, Down, Left, Right ]
                |> List.foldl
                    (placedBombeBehavoiur currentLocation)
                    game
                |> Tuple.mapSecond
                    (Dict.update currentLocation (always (Just (Effect Smoke))))

        _ ->
            game


placedBombeBehavoiur : Map.Location -> Direction -> Game -> Game
placedBombeBehavoiur currentLocation dir game =
    let
        newLocation =
            Map.dirCoordinates dir |> Pair.map2 (+) currentLocation
    in
    case game |> Tuple.second |> Dict.get newLocation of
        Just (Enemy _ _) ->
            game
                |> Tuple.mapSecond
                    (Dict.update newLocation (always (Just (Item (Miscellaneous Bone)))))

        Nothing ->
            game
                |> Tuple.mapSecond
                    (Dict.update newLocation (always (Just (Effect Smoke))))

        _ ->
            game
