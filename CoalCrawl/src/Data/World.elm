module Data.World exposing (..)

import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Item exposing (Item)
import Dict exposing (Dict)


type alias World =
    { floor : Dict ( Int, Int ) Floor
    , items : Dict ( Int, Int ) (List Item)
    , entities : Dict ( Int, Int ) Entity
    , actors : Dict Int ( ( Int, Int ), Actor )
    , nextId : Int
    }


empty : World
empty =
    { floor = Dict.empty
    , items = Dict.empty
    , entities = Dict.empty
    , actors = Dict.empty
    , nextId = 0
    }


fromList : List ( ( Int, Int ), Block ) -> World
fromList =
    List.foldl (\( p, b ) -> insert p b) empty


insert : ( Int, Int ) -> Block -> World -> World
insert pos block world =
    case block of
        Data.Block.FloorBlock floor ->
            insertFloorAt pos floor world

        Data.Block.EntityBlock entity ->
            insertEntityAt pos entity world


insertItem : Item -> ( Int, Int ) -> World -> World
insertItem item pos =
    insertItemAt pos item


insertItemAt : ( Int, Int ) -> Item -> World -> World
insertItemAt pos item world =
    { world
        | items =
            world.items
                |> Dict.update pos
                    (\maybe ->
                        maybe
                            |> Maybe.withDefault []
                            |> (::) item
                            |> Just
                    )
        , floor =
            world.floor
                |> Dict.update pos
                    (\floor ->
                        floor
                            |> Maybe.withDefault Data.Floor.Ground
                            |> Just
                    )
    }


insertFloor : Floor -> ( Int, Int ) -> World -> World
insertFloor floor pos =
    insertFloorAt pos floor


insertFloorAt : ( Int, Int ) -> Floor -> World -> World
insertFloorAt pos floor world =
    { world | floor = world.floor |> Dict.insert pos floor }


insertEntity : Entity -> ( Int, Int ) -> World -> World
insertEntity entity pos =
    insertEntityAt pos entity


insertEntityAt : ( Int, Int ) -> Entity -> World -> World
insertEntityAt pos entity world =
    world
        |> removeEntity pos
        |> (\w ->
                { w
                    | entities = world.entities |> Dict.insert pos entity
                }
           )


insertActor : Actor -> ( Int, Int ) -> World -> World
insertActor actor pos =
    insertActorAt pos actor


insertActorAt : ( Int, Int ) -> Actor -> World -> World
insertActorAt pos actor world =
    world
        |> removeEntity pos
        |> (\w ->
                { w
                    | entities =
                        world.entities
                            |> Dict.insert pos (Data.Entity.Actor world.nextId)
                    , actors =
                        world.actors
                            |> Dict.insert world.nextId ( pos, actor )
                    , nextId = world.nextId + 1
                }
           )


removeItem : ( Int, Int ) -> World -> World
removeItem pos world =
    { world
        | items =
            world.items
                |> Dict.update pos
                    (Maybe.andThen
                        (\maybe ->
                            case maybe of
                                [ _ ] ->
                                    Nothing

                                _ :: tail ->
                                    Just tail

                                [] ->
                                    Nothing
                        )
                    )
    }


removeFloor : ( Int, Int ) -> World -> World
removeFloor pos world =
    { world
        | floor = world.floor |> Dict.insert pos Data.Floor.Ground
    }


removeEntity : ( Int, Int ) -> World -> World
removeEntity pos world =
    { world
        | entities = world.entities |> Dict.remove pos
        , floor =
            world.floor
                |> Dict.update pos
                    (\maybe ->
                        maybe
                            |> Maybe.withDefault Data.Floor.Ground
                            |> Just
                    )
        , actors =
            case world.entities |> Dict.get pos of
                Just (Data.Entity.Actor id) ->
                    world.actors |> Dict.remove id

                _ ->
                    world.actors
    }


updateEntity : ( Int, Int ) -> (Maybe Entity -> Maybe Entity) -> World -> World
updateEntity pos fun world =
    { world | entities = world.entities |> Dict.update pos fun }


{-| gets entites first, floor second
-}
get : ( Int, Int ) -> World -> Maybe ( Block, Maybe Item )
get pos world =
    let
        items =
            world.items
                |> Dict.get pos
                |> Maybe.andThen List.head
    in
    world
        |> getBlock pos
        |> Maybe.map (\block -> ( block, items ))


getBlock : ( Int, Int ) -> World -> Maybe Block
getBlock pos world =
    case world.entities |> Dict.get pos of
        Just a ->
            Just (Data.Block.EntityBlock a)

        Nothing ->
            getFloor pos world
                |> Maybe.map Data.Block.FloorBlock


getActorAt : ( Int, Int ) -> World -> Maybe ( Int, Actor )
getActorAt pos world =
    world.entities
        |> Dict.get pos
        |> Maybe.andThen
            (\entity ->
                case entity of
                    Data.Entity.Actor id ->
                        world.actors
                            |> Dict.get id
                            |> Maybe.map (\( _, actor ) -> ( id, actor ))

                    _ ->
                        Nothing
            )


getActor : Int -> World -> Maybe ( ( Int, Int ), Actor )
getActor id world =
    world.actors
        |> Dict.get id


updateActor : Int -> (Actor -> Actor) -> World -> World
updateActor id fun world =
    world.actors
        |> Dict.update id (Maybe.map (Tuple.mapSecond fun))
        |> (\actors -> { world | actors = actors })


moveActorTo : ( Int, Int ) -> Int -> World -> World
moveActorTo pos id world =
    world.actors
        |> Dict.get id
        |> Maybe.map
            (\( oldPos, _ ) ->
                { world
                    | entities =
                        world.entities
                            |> Dict.remove oldPos
                            |> Dict.insert pos (Data.Entity.Actor id)
                    , actors =
                        world.actors
                            |> Dict.update id
                                (Maybe.map (Tuple.mapFirst (\_ -> pos)))
                }
            )
        |> Maybe.withDefault world


getFloor : ( Int, Int ) -> World -> Maybe Floor
getFloor pos world =
    world.floor
        |> Dict.get pos


getEntities : World -> List ( ( Int, Int ), Entity )
getEntities world =
    world.entities |> Dict.toList


getActors : World -> List ( Int, ( ( Int, Int ), Actor ) )
getActors world =
    world.actors |> Dict.toList
