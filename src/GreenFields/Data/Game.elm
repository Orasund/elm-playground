module GreenFields.Data.Game exposing (Game, applyEffect, build, canBuild, canRestore, demolish, distanceBetweenTowns, empty, fromBoard, getBoard, getNeighbors, getResources, getTile, hasNoActives, isActive, restore, update, viewRadius)

import Bag exposing (Bag)
import Dict exposing (Dict)
import Firestore exposing (Error)
import GreenFields.Data.Building as Building exposing (Building(..), restoringCost)
import GreenFields.Data.Database as Database
import GreenFields.Data.Effect exposing (Effect(..))
import GreenFields.Data.Resource as Resource exposing (Resource(..))
import GreenFields.Data.Tile as Tile exposing (Tile)
import Html
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Posix)


applyEffect : Effect -> Game -> Game
applyEffect effect (Game game) =
    case effect of
        IncreaseInventorySpace int ->
            Game { game | inventorySize = max int game.inventorySize }


viewRadius : Int
viewRadius =
    3


distanceBetweenTowns : Int
distanceBetweenTowns =
    16


type Game
    = Game
        { board : Dict ( Int, Int ) Tile
        , activeTiles : Set ( Int, Int )
        , resources : Bag String
        , inventorySize : Int
        }


getResources : Game -> List ( Resource, Int )
getResources (Game game) =
    game.resources
        |> Bag.toList
        |> List.map (Tuple.mapFirst Resource.fromString)


empty : Game
empty =
    fromBoard Dict.empty


fromBoard : Dict ( Int, Int ) Tile -> Game
fromBoard board =
    Game
        { board = board
        , activeTiles = Set.empty
        , resources =
            [ ( Wood |> Resource.toString, 3 ) ]
                |> Bag.fromList
        , inventorySize = 6
        }


getBoard : Game -> Dict ( Int, Int ) Tile
getBoard (Game game) =
    game.board


update : Tile -> Game -> Game
update tile (Game game) =
    Game
        { game
            | board =
                game.board
                    |> Dict.insert ( tile.x, tile.y ) tile
        }


getNeighbors : ( Int, Int ) -> Game -> List ( Tile, ( Int, Int ) )
getNeighbors ( x, y ) (Game game) =
    [ ( 1, 0 )
    , ( 0, 1 )
    , ( -1, 0 )
    , ( 0, -1 )
    ]
        |> List.filterMap
            (Tuple.mapBoth ((+) x) ((+) y)
                >> (\pos ->
                        game.board
                            |> Dict.get pos
                            |> Maybe.map (\tile -> ( tile, pos ))
                   )
            )


hasNoActives : Game -> Bool
hasNoActives (Game game) =
    game.activeTiles |> Set.isEmpty


isActive : ( Int, Int ) -> Game -> Bool
isActive pos (Game game) =
    game.activeTiles |> Set.member pos


getTile : ( Int, Int ) -> Game -> Maybe Tile
getTile ( x, y ) (Game game) =
    case game.board |> Dict.get ( x, y ) of
        Just tile ->
            Just tile

        Nothing ->
            if
                (x
                    |> modBy distanceBetweenTowns
                    |> (==) 0
                )
                    && (y
                            |> modBy distanceBetweenTowns
                            |> (==) 0
                       )
            then
                Just <| Tile.generate ( x, y )

            else
                Nothing


internalIsSubbagOf : Bag comparable -> Bag comparable -> Bool
internalIsSubbagOf bagA bagB =
    bagB
        |> Bag.toList
        |> List.all (\( b, n ) -> n <= (bagA |> Bag.count b))


canRestore : ( Int, Int ) -> Game -> Bool
canRestore position (Game game) =
    let
        tile =
            game.board
                |> Dict.get position
                |> Maybe.withDefault (Tile.generate position)

        restoringCost =
            tile.building
                |> Building.restoringCost

        hasEnoughSpace =
            restoringCost
                |> Bag.foldl
                    (\resource n -> Bag.remove n resource)
                    game.resources
                |> (\resources ->
                        tile.building
                            |> Building.produces
                            |> Bag.foldl
                                (\resource n -> Bag.insert n resource)
                                resources
                   )
                |> Bag.size
                |> (>=) game.inventorySize
    in
    (restoringCost |> internalIsSubbagOf game.resources)
        && hasEnoughSpace


restore :
    { position : ( Int, Int ), timestamp : Posix }
    -> Game
    -> ( Maybe (Task Error Tile), Game )
restore arg (Game game) =
    let
        tile =
            game.board
                |> Dict.get arg.position
                |> Maybe.withDefault (Tile.generate arg.position)

        restoringCost =
            tile.building
                |> Building.restoringCost
    in
    if Game game |> canRestore arg.position then
        ( Just (Database.insertTile { tile | timestamp = arg.timestamp })
        , Game
            { game
                | activeTiles = game.activeTiles |> Set.insert arg.position
                , board =
                    game.board
                        |> Dict.insert arg.position
                            { tile | timestamp = arg.timestamp }
                , resources =
                    restoringCost
                        |> Bag.foldl
                            (\resource n -> Bag.remove n resource)
                            game.resources
                        |> (\resources ->
                                tile.building
                                    |> Building.produces
                                    |> Bag.foldl
                                        (\resource n -> Bag.insert n resource)
                                        resources
                           )
            }
            |> (\g ->
                    tile.building
                        |> Building.effects
                        |> List.foldl
                            applyEffect
                            g
               )
        )

    else
        ( Nothing, Game game )


canBuild :
    Building
    -> Game
    -> Bool
canBuild building (Game game) =
    let
        buildingCost =
            building
                |> Building.buildingCost

        hasEnoughSpace =
            buildingCost
                |> Bag.foldl
                    (\resource n -> Bag.remove n resource)
                    game.resources
                |> (\resources ->
                        building
                            |> Building.produces
                            |> Bag.foldl
                                (\resource n -> Bag.insert n resource)
                                resources
                   )
                |> Bag.size
                |> (>=) game.inventorySize
    in
    (buildingCost |> internalIsSubbagOf game.resources)
        && hasEnoughSpace


build :
    { building : Building
    , position : ( Int, Int )
    , timestamp : Posix
    }
    -> Game
    -> ( Maybe (Task Error Tile), Game )
build arg (Game game) =
    let
        buildingCost =
            arg.building
                |> Building.buildingCost

        ( x, y ) =
            arg.position

        tile =
            { building = arg.building
            , timestamp = arg.timestamp
            , x = x
            , y = y
            }

        oldBuilding =
            game.board |> Dict.get arg.position
    in
    if Game game |> canBuild arg.building then
        ( Just (Database.insertTile tile)
        , Game
            { game
                | activeTiles = game.activeTiles |> Set.insert arg.position
                , board = game.board |> Dict.insert arg.position tile
                , resources =
                    buildingCost
                        |> Bag.toList
                        |> List.foldl
                            (\( resource, n ) -> Bag.remove n resource)
                            game.resources
                        |> (\resources ->
                                (case oldBuilding of
                                    Just old ->
                                        Building.upgradedProduction
                                            { old = old.building
                                            , new = tile.building
                                            }

                                    Nothing ->
                                        tile.building
                                            |> Building.produces
                                )
                                    |> Bag.foldl
                                        (\resource n -> Bag.insert n resource)
                                        resources
                           )
            }
            |> (\g ->
                    arg.building
                        |> Building.effects
                        |> List.foldl applyEffect g
               )
        )

    else
        ( Nothing, Game game )


demolish : ( Int, Int ) -> Game -> ( Task Error (), Game )
demolish pos (Game game) =
    ( Database.deleteTile pos
    , Game
        { game
            | board =
                game.board
                    |> Dict.remove pos
        }
    )
