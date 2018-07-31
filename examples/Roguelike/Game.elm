module Roguelike.Game exposing (applyDirection)

import Dict
import Pair
import Roguelike.Cell as Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , EffectType(..)
        , EnemyType(..)
        , Item(..)
        , MiscellaneousType(..)
        , SolidType(..)
        )
import Roguelike.Map as Map exposing (Direction(..), Location, Map)
import Roguelike.Player as Player exposing (Game, PlayerCell)


applyDirection : Int -> Direction -> ( PlayerCell, Game ) -> ( PlayerCell, Game )
applyDirection size dir (( ( location, direction ), ( playerData, map ) ) as playerCellAndGame) =
    if direction == dir then
        playerCellAndGame
            |> Player.move size
            |> (\( newPlayerCell, game ) ->
                    ( newPlayerCell
                    , updateGame newPlayerCell game
                    )
               )
    else
        playerCellAndGame
            |> Tuple.mapSecond (Tuple.mapSecond (Player.face location dir))


updateGame : PlayerCell -> Game -> Game
updateGame playerCell (( _, map ) as game) =
    map
        |> Dict.foldl
            (updateCell playerCell)
            game


updateCell : PlayerCell -> Location -> Cell -> Game -> Game
updateCell playerCell location cell =
    case cell of
        Enemy enemy _ ->
            updateEnemy location enemy playerCell

        Effect _ ->
            Tuple.mapSecond (Dict.remove location)

        _ ->
            identity


updateEnemy : Location -> EnemyType -> PlayerCell -> Game -> Game
updateEnemy location enemyType playerCell =
    attackPlayer location playerCell
        >> specialBehaviour location enemyType playerCell


attackPlayer : Location -> PlayerCell -> Game -> Game
attackPlayer location (( playerLocation, _ ) as playerCell) ( playerData, map ) =
    [ Up, Down, Left, Right ]
        |> List.filter
            ((==) (Pair.map2 (-) location playerLocation) << Map.dirCoordinates)
        |> List.head
        |> Maybe.map (always (( playerData, map ) |> Player.attack playerCell))
        |> Maybe.withDefault ( playerData, map )


specialBehaviour : Location -> EnemyType -> PlayerCell -> Game -> Game
specialBehaviour currentLocation enemyType ( playerLocation, _ ) game =
    let
        map : Map Cell
        map =
            game |> Tuple.second
    in
    case enemyType of
        PlacedBombe ->
            [ Up, Down, Left, Right ]
                |> List.foldl
                    (placedBombeBehavoiur currentLocation)
                    game
                |> Tuple.mapSecond
                    (Dict.update currentLocation (always (Just (Effect Smoke))))

        monster ->
            let
                moveDirection : Direction
                moveDirection =
                    Pair.map2 (-) playerLocation currentLocation
                        --|> (\( x, y ) -> ( y, x ))
                        |> Map.approximateDirection

                newLocation : Location
                newLocation =
                    Pair.map2 (+) currentLocation (Map.dirCoordinates moveDirection)
            in
            case map |> Dict.get newLocation of
                Nothing ->
                    game |> Tuple.mapSecond (Map.move currentLocation moveDirection)

                Just (Item _) ->
                    game |> Tuple.mapSecond (Map.move currentLocation moveDirection)

                Just (Solid solid) ->
                    if
                        Cell.resistancy solid
                            <= (case monster of
                                    PlacedBombe ->
                                        0

                                    Oger ->
                                        3

                                    Goblin ->
                                        2

                                    Rat ->
                                        1
                               )
                    then
                        game
                            |> Tuple.mapSecond
                                (Dict.update newLocation (always (Cell.decomposing solid |> Tuple.first |> Maybe.map Solid)))
                    else
                        game

                _ ->
                    game


placedBombeBehavoiur : Location -> Direction -> Game -> Game
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
