module Data.Entity exposing (..)

import Config
import Data.Item exposing (Item)
import Data.Storage exposing (Storage)


type Entity
    = Vein Item
    | Wall
    | CrackedWall
    | Water
    | Lava
    | Container Storage


container : Entity
container =
    Data.Storage.empty Config.containerMaxItems
        |> Container
