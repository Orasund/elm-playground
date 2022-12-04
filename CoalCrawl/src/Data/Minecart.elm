module Data.Minecart exposing (..)

import Config
import Data.Item exposing (Item)
import Data.Sound exposing (Sound)
import Data.Storage exposing (Storage)


type alias Minecart =
    { storage : Storage
    , movedFrom : Maybe ( Int, Int )
    }


emptyWagon : Minecart
emptyWagon =
    { storage = Data.Storage.empty Config.wagonMaxItems
    , movedFrom = Nothing
    }


fullWagon : Item -> Minecart
fullWagon item =
    { emptyWagon
        | storage = Data.Storage.full Config.wagonMaxItems item
    }


setStorageOf : Minecart -> Storage -> Minecart
setStorageOf minecart storage =
    { minecart | storage = storage }


insert : Item -> Minecart -> Maybe ( Minecart, Sound )
insert item minecart =
    minecart.storage
        |> Data.Storage.insert item
        |> Maybe.map
            (\storage ->
                ( setStorageOf minecart storage
                , Data.Sound.PickUp
                )
            )


moveFrom : ( Int, Int ) -> Minecart -> Minecart
moveFrom movedFrom wagon =
    { wagon | movedFrom = Just movedFrom }


stop : Minecart -> Minecart
stop wagon =
    { wagon | movedFrom = Nothing }


isFull : Minecart -> Bool
isFull wagon =
    Data.Storage.isFull wagon.storage


load : List Item -> Minecart -> Maybe Minecart
load items minecart =
    minecart.storage
        |> Data.Storage.load items
        |> Maybe.map (setStorageOf minecart)


unload : Minecart -> ( Minecart, List Item )
unload minecart =
    minecart.storage
        |> Data.Storage.unload
        |> Tuple.mapFirst (setStorageOf minecart)
