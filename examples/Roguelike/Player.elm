module Roguelike.Player exposing (PlayerData, activate, attack, drop, face, getCell, init, move, rotateLeft, rotateRight)

import Dict exposing (Dict)
import Roguelike.Cell as Cell exposing (Cell(..), ConsumableType(..), Direction(..), EnemyType(..), Item(..), SolidType(..))
import Roguelike.Inventory as Inventory exposing (Inventory)
import Roguelike.Map as Map exposing (Map)


type alias PlayerData =
    { inventory : Inventory Item
    , lifes : Int
    }


init : Int -> PlayerData
init backpackSize =
    { inventory = Inventory.init backpackSize
    , lifes = 3
    }


getCell : Map Cell -> Maybe ( Map.Location, Direction )
getCell map =
    map
        |> Map.getUnique
            (\key cell ->
                case cell of
                    Player _ ->
                        True

                    _ ->
                        False
            )
        |> Maybe.andThen
            (\( key, cell ) ->
                case cell of
                    Player dir ->
                        Just ( key, dir )

                    _ ->
                        Nothing
            )


face : Map.Location -> Direction -> Map Cell -> Map Cell
face location direction map =
    map |> Map.place location (Player direction)


attack : PlayerData -> PlayerData
attack player =
    { player | lifes = player.lifes - 1 }


move : Int -> Map.Location -> Direction -> ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
move worldSize ( x, y ) direction ( playerData, map ) =
    let
        ( i, j ) =
            Map.dirCoordinates direction

        outOfBound : Bool
        outOfBound =
            case direction of
                Up ->
                    y == 0

                Down ->
                    y == worldSize

                Left ->
                    x == worldSize

                Right ->
                    x == 0

        move : ( Int, Int ) -> ( Int, Int ) -> Map Cell -> Map Cell
        move ( x, y ) ( i, j ) map =
            case map |> Dict.get ( x, y ) of
                Just cell ->
                    map
                        |> Dict.update ( x + i, y + j ) (always (Just cell))
                        |> Dict.remove ( x, y )

                Nothing ->
                    map
    in
    if outOfBound then
        ( playerData, map )
    else
        case map |> Dict.get ( x + i, y + j ) of
            Just (Item a) ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( playerData
                    |> (\b ->
                            { b
                                | inventory = inventory |> Inventory.add a
                            }
                       )
                , map
                    |> move ( x, y ) ( i, j )
                    |> (\m ->
                            case item of
                                Just a ->
                                    m |> Map.place ( x, y ) (Item a)

                                _ ->
                                    m
                       )
                )

            Just (Enemy a) ->
                ( playerData
                , case map |> Dict.get ( x + i * 2, y + j * 2 ) of
                    Just (Solid _) ->
                        map

                    Just (Enemy _) ->
                        map

                    _ ->
                        case map |> Dict.get ( x + i * 3, y + j * 3 ) of
                            Just (Solid _) ->
                                map
                                    |> move ( x + i, y + j ) ( i, j )

                            Just (Enemy _) ->
                                map
                                    |> move ( x + i, y + j ) ( i, j )

                            _ ->
                                map
                                    |> move ( x + i, y + j ) ( i * 2, j * 2 )
                )

            Nothing ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( playerData |> (\a -> { a | inventory = inventory })
                , map
                    |> move ( x, y ) ( i, j )
                    |> (\m ->
                            case item of
                                Just a ->
                                    m |> Map.place ( x, y ) (Item a)

                                Nothing ->
                                    m
                       )
                )

            Just (Effect _) ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( playerData |> (\a -> { a | inventory = inventory })
                , map
                    |> move ( x, y ) ( i, j )
                    |> (\m ->
                            case item of
                                Just a ->
                                    m |> Map.place ( x, y ) (Item a)

                                Nothing ->
                                    m
                       )
                )

            Just (Solid PlacedDirt) ->
                ( playerData
                    |> (\pd ->
                            { pd
                                | inventory =
                                    pd.inventory
                                        |> Inventory.add (Consumable Dirt)
                            }
                       )
                , map
                    |> Dict.remove ( x + i, y + j )
                )

            _ ->
                ( playerData
                , map
                    |> face ( x, y ) direction
                )


activate : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
activate ( playerData, map ) =
    case playerData.inventory |> Inventory.selected of
        Just (Consumable a) ->
            ( playerData
                |> (\b ->
                        { b
                            | inventory =
                                playerData.inventory
                                    |> Inventory.take
                                    |> Tuple.second
                        }
                   )
            , map
            )
                |> (\tuple ->
                        case a of
                            Bombe ->
                                let
                                    pos : ( Int, Int )
                                    pos =
                                        case getCell (tuple |> Tuple.second) of
                                            Just ( ( x, y ), dir ) ->
                                                let
                                                    ( i, j ) =
                                                        Map.dirCoordinates dir
                                                in
                                                ( x + i, y + j )

                                            Nothing ->
                                                ( 0, 0 )
                                in
                                case tuple |> Tuple.second |> Dict.get pos of
                                    Nothing ->
                                        tuple
                                            |> Tuple.mapSecond (Map.place pos (Enemy PlacedBombe))

                                    Just (Effect _) ->
                                        tuple
                                            |> Tuple.mapSecond (Map.place pos (Enemy PlacedBombe))

                                    Just (Solid DirtWall) ->
                                        ( tuple
                                            |> Tuple.first
                                            |> (\pd ->
                                                    { pd
                                                        | inventory =
                                                            pd.inventory
                                                                |> Inventory.add (Consumable Dirt)
                                                    }
                                               )
                                        , tuple
                                            |> Tuple.second
                                            |> Map.place pos (Solid PlacedDirt)
                                        )

                                    _ ->
                                        tuple

                            Dirt ->
                                let
                                    pos : ( Int, Int )
                                    pos =
                                        case getCell (tuple |> Tuple.second) of
                                            Just ( ( x, y ), dir ) ->
                                                let
                                                    ( i, j ) =
                                                        Map.dirCoordinates dir
                                                in
                                                ( x + i, y + j )

                                            Nothing ->
                                                ( 0, 0 )
                                in
                                case tuple |> Tuple.second |> Dict.get pos of
                                    Nothing ->
                                        tuple
                                            |> Tuple.mapSecond (Map.place pos (Solid PlacedDirt))

                                    Just (Effect _) ->
                                        tuple
                                            |> Tuple.mapSecond (Map.place pos (Solid PlacedDirt))

                                    Just (Solid PlacedDirt) ->
                                        tuple
                                            |> Tuple.mapSecond (Map.place pos (Solid DirtWall))

                                    _ ->
                                        tuple

                            _ ->
                                tuple
                   )

        _ ->
            ( playerData, map )


drop : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
drop ( playerData, map ) =
    let
        ( item, inventory ) =
            playerData.inventory |> Inventory.take

        dir : Map.Location
        dir =
            case getCell map of
                Just ( ( x, y ), dir ) ->
                    let
                        ( i, j ) =
                            Map.dirCoordinates dir
                    in
                    ( x + i, y + j )

                Nothing ->
                    ( 0, 0 )
    in
    case map |> Dict.get dir of
        Nothing ->
            ( playerData |> (\a -> { a | inventory = inventory })
            , case item of
                Just a ->
                    map |> Map.place dir (Item a)

                _ ->
                    map
            )

        _ ->
            ( playerData, map )


rotateLeft : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
rotateLeft ( playerData, map ) =
    ( { playerData
        | inventory =
            playerData.inventory
                |> Inventory.rotateLeft
      }
    , map
    )


rotateRight : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
rotateRight ( playerData, map ) =
    ( { playerData
        | inventory =
            playerData.inventory
                |> Inventory.rotateRight
      }
    , map
    )
