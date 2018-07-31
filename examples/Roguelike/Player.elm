module Roguelike.Player
    exposing
        ( Game
        , PlayerCell
        , PlayerData
        , activate
        , attack
        , drop
        , face
        , init
        , move
        , rotateLeft
        , rotateRight
        )

import Dict
import Pair
import Roguelike.Cell as Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , EnemyType(..)
        , Item(..)
        , MaterialType(..)
        , MiscellaneousType(..)
        , SolidType(..)
        )
import Roguelike.Inventory as Inventory exposing (Inventory)
import Roguelike.Map as Map exposing (Direction(..), Location, Map)


type alias PlayerData =
    { inventory : Inventory Item
    , lifes : Int
    }


type alias PlayerCell =
    ( Location, Direction )


type alias Game =
    ( PlayerData, Map Cell )


init : Int -> PlayerData
init backpackSize =
    { inventory = Inventory.init backpackSize
    , lifes = 3
    }


face : Location -> Direction -> Map Cell -> Map Cell
face location direction map =
    map |> Map.place location (Player direction)


attack : PlayerCell -> Game -> Game
attack ( location, _ ) ( playerData, currentMap ) =
    let
        lifes : Int
        lifes =
            playerData.lifes - 1
    in
    ( { playerData | lifes = lifes }
    , currentMap
        |> (if lifes > 0 then
                identity
            else
                Dict.update location (always (Just (Item (Miscellaneous Bone))))
           )
    )


move : Int -> ( PlayerCell, Game ) -> ( PlayerCell, Game )
move worldSize ( ( location, direction ) as playerCell, ( playerData, currentMap ) as game ) =
    let
        moveDir =
            Map.dirCoordinates direction

        outOfBound : Bool
        outOfBound =
            location
                |> (\( x, y ) ->
                        case direction of
                            Up ->
                                y == 0

                            Down ->
                                y == worldSize

                            Left ->
                                x == 0

                            Right ->
                                x == worldSize
                   )

        newLocation : Location
        newLocation =
            Pair.map2 (+) location moveDir

        newPlayerCell : PlayerCell
        newPlayerCell =
            playerCell |> Tuple.mapFirst (always newLocation)
    in
    if outOfBound then
        ( playerCell, game )
    else
        case currentMap |> Dict.get newLocation of
            Just (Item item) ->
                let
                    ( maybeDroppedItem, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( newPlayerCell
                , ( { playerData
                        | inventory = inventory |> Inventory.add item
                    }
                  , currentMap
                        |> Map.move location direction
                        |> (case maybeDroppedItem of
                                Just droppedItem ->
                                    Map.place location (Item droppedItem)

                                _ ->
                                    identity
                           )
                  )
                )

            Just (Enemy _ _) ->
                ( playerCell
                , ( playerData
                  , case
                        currentMap
                            |> Dict.get
                                ((moveDir
                                    |> Pair.map ((*) 2)
                                 )
                                    |> Pair.map2 (+) location
                                )
                    of
                        Just (Solid _) ->
                            currentMap

                        Just (Enemy _ _) ->
                            currentMap

                        _ ->
                            currentMap
                                |> Map.move newLocation direction
                                |> (case currentMap |> Dict.get ((moveDir |> Pair.map ((*) 3)) |> Pair.map2 (+) location) of
                                        Just (Solid _) ->
                                            identity

                                        Just (Enemy _ _) ->
                                            identity

                                        _ ->
                                            Map.move (Pair.map2 (+) location (Pair.map ((*) 2) moveDir)) direction
                                   )
                  )
                )

            Nothing ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( newPlayerCell
                , ( playerData |> (\a -> { a | inventory = inventory })
                  , currentMap
                        |> Map.move location direction
                        |> (\m ->
                                case item of
                                    Just a ->
                                        m |> Map.place location (Item a)

                                    Nothing ->
                                        m
                           )
                  )
                )

            Just (Effect _) ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( newPlayerCell
                , ( playerData |> (\a -> { a | inventory = inventory })
                  , currentMap
                        |> Map.move location direction
                        |> (\m ->
                                case item of
                                    Just a ->
                                        m |> Map.place location (Item a)

                                    Nothing ->
                                        m
                           )
                  )
                )

            Just (Solid solid) ->
                ( playerCell
                , case Cell.decomposing solid of
                    ( Nothing, _ ) ->
                        ( playerData
                            |> addToInventory (Consumable (Material Dirt))
                        , currentMap
                            |> Dict.remove newLocation
                        )

                    _ ->
                        game |> Tuple.mapSecond (face location direction)
                )

            _ ->
                ( playerCell
                , game |> Tuple.mapSecond (face location direction)
                )


activate : PlayerCell -> Game -> Game
activate playerCell (( playerData, _ ) as game) =
    game
        |> (case playerData |> .inventory |> Inventory.selected of
                Just (Consumable consumable) ->
                    Tuple.mapFirst takeFromInventory
                        >> consumableAction playerCell consumable

                _ ->
                    drop playerCell
           )


consumableAction : PlayerCell -> ConsumableType -> Game -> Game
consumableAction playerCell consumable (( playerData, map ) as game) =
    let
        defaultCase : Game -> Game
        defaultCase =
            Tuple.mapFirst (addToInventory (Consumable consumable))
    in
    game
        |> ((case consumable of
                Bombe ->
                    bombeAction map playerCell

                Material material ->
                    materialAction map playerCell material

                HealthPotion ->
                    healthPotionAction playerData
            )
                |> Maybe.withDefault defaultCase
           )


healthPotionAction : PlayerData -> Maybe (Game -> Game)
healthPotionAction { lifes } =
    if lifes < 3 then
        Just
            (\game ->
                game
                    |> Tuple.mapFirst
                        (\playerData ->
                            { playerData | lifes = lifes + 1 }
                        )
            )
    else
        Nothing


bombeAction : Map Cell -> PlayerCell -> Maybe (Game -> Game)
bombeAction currentMap playerCell =
    let
        specialCase : SolidType -> Maybe (Game -> Game)
        specialCase solidType =
            let
                ( maybeSolid, material ) =
                    Cell.decomposing solidType
            in
            maybeSolid
                |> Maybe.map
                    (\solid ->
                        \( playerData, map ) ->
                            ( playerData
                                |> addToInventory (Consumable (Material material))
                            , map
                                |> Map.place (posFront playerCell) (Solid solid)
                            )
                    )

        --|> Maybe.withDefault game
    in
    placingItem currentMap playerCell (Enemy PlacedBombe "") specialCase


materialAction : Map Cell -> PlayerCell -> MaterialType -> Maybe (Game -> Game)
materialAction map playerCell material =
    let
        specialCase : SolidType -> Maybe (Game -> Game)
        specialCase solidType =
            Just
                (case Cell.composing ( Just solidType, material ) of
                    Just newSolid ->
                        Tuple.mapSecond (Map.place (posFront playerCell) (Solid newSolid))

                    Nothing ->
                        Tuple.mapFirst (addToInventory (Consumable (Material material)))
                )
    in
    Cell.composing ( Nothing, material )
        |> Maybe.andThen
            (\solid ->
                placingItem
                    map
                    playerCell
                    (Solid solid)
                    specialCase
            )


placingItem : Map Cell -> PlayerCell -> Cell -> (SolidType -> Maybe (Game -> Game)) -> Maybe (Game -> Game)
placingItem map playerCell cell specialCase =
    let
        frontPos : Map.Location
        frontPos =
            posFront playerCell
    in
    case map |> Dict.get frontPos of
        Nothing ->
            Just (Tuple.mapSecond (Map.place frontPos cell))

        Just (Effect _) ->
            Just (Tuple.mapSecond (Map.place frontPos cell))

        Just (Solid solidType) ->
            specialCase solidType

        _ ->
            Nothing


posFront : PlayerCell -> Location
posFront ( location, direction ) =
    Pair.map2 (+) location (direction |> Map.dirCoordinates)


drop : PlayerCell -> Game -> Game
drop ( location, direction ) ( playerData, map ) =
    let
        ( item, inventory ) =
            playerData.inventory |> Inventory.take

        dir : Map.Location
        dir =
            Pair.map2 (+) location (Map.dirCoordinates direction)
    in
    case map |> Dict.get dir of
        Nothing ->
            ( { playerData | inventory = inventory }
            , map
                |> (item
                        |> Maybe.map (\a -> Map.place dir (Item a))
                        |> Maybe.withDefault identity
                   )
            )

        _ ->
            ( playerData, map )


takeFromInventory : PlayerData -> PlayerData
takeFromInventory ({ inventory } as playerData) =
    { playerData
        | inventory = inventory |> Inventory.take |> Tuple.second
    }


addToInventory : Item -> PlayerData -> PlayerData
addToInventory item ({ inventory } as playerData) =
    { playerData
        | inventory =
            inventory
                |> Inventory.add item
    }


rotateLeft : PlayerData -> PlayerData
rotateLeft ({ inventory } as playerData) =
    { playerData
        | inventory = inventory |> Inventory.rotateLeft
    }


rotateRight : PlayerData -> PlayerData
rotateRight ({ inventory } as playerData) =
    { playerData
        | inventory = inventory |> Inventory.rotateRight
    }
