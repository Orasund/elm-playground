module Data.World exposing (..)

import Data.Block exposing (Block)
import Data.Entity exposing (Entity(..))
import Data.Floor exposing (Floor)
import Dict exposing (Dict)


type alias World =
    { floor : Dict ( Int, Int ) Floor
    , entities : Dict ( Int, Int ) Entity
    }


empty : World
empty =
    { floor = Dict.empty
    , entities = Dict.empty
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


getFloor : ( Int, Int ) -> World -> Maybe Floor
getFloor pos world =
    world.floor
        |> Dict.get pos
