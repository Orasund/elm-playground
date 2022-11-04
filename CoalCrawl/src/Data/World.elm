module Data.World exposing (..)

import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Entity exposing (Entity(..))
import Data.Floor exposing (Floor)
import Dict exposing (Dict)


type alias World =
    { floor : Dict ( Int, Int ) Floor
    , entities : Dict ( Int, Int ) Entity
    , actors : Dict Int ( ( Int, Int ), Actor )
    , nextId : Int
    }


empty : World
empty =
    { floor = Dict.empty
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
            insertFloor pos floor world

        Data.Block.EntityBlock entity ->
            insertEntity pos entity world


insertFloor : ( Int, Int ) -> Floor -> World -> World
insertFloor pos floor world =
    { world | floor = world.floor |> Dict.insert pos floor }


insertEntity : ( Int, Int ) -> Entity -> World -> World
insertEntity pos entity world =
    { world
        | entities = world.entities |> Dict.insert pos entity
    }


insertActor : ( Int, Int ) -> Actor -> World -> World
insertActor pos actor world =
    { world
        | entities =
            world.entities
                |> Dict.insert pos (Data.Entity.Actor world.nextId)
        , actors =
            world.actors
                |> Dict.insert world.nextId ( pos, actor )
        , nextId = world.nextId + 1
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
                            |> Maybe.withDefault Data.Floor.ground
                            |> Just
                    )
    }


updateEntity : ( Int, Int ) -> (Maybe Entity -> Maybe Entity) -> World -> World
updateEntity pos fun world =
    { world | entities = world.entities |> Dict.update pos fun }


update : ( Int, Int ) -> (Maybe Block -> Maybe Block) -> World -> World
update pos fun world =
    world
        |> get pos
        |> fun
        |> Maybe.withDefault (Data.Block.FloorBlock Data.Floor.ground)
        |> (\block -> insert pos block world)


{-| gets entites first, floor second
-}
get : ( Int, Int ) -> World -> Maybe Block
get pos world =
    case world.entities |> Dict.get pos of
        Just a ->
            Just (Data.Block.EntityBlock a)

        Nothing ->
            getFloor pos world
                |> Maybe.map Data.Block.FloorBlock


getActor : ( Int, Int ) -> World -> Maybe Actor
getActor pos world =
    world.entities
        |> Dict.get pos
        |> Maybe.andThen
            (\entity ->
                case entity of
                    Data.Entity.Actor id ->
                        world.actors
                            |> Dict.get id
                            |> Maybe.map Tuple.second

                    _ ->
                        Nothing
            )


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
                            |> Dict.insert pos (Data.Entity.Actor id)
                            |> Dict.remove oldPos
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
