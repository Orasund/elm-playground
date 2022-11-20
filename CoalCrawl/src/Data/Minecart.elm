module Data.Minecart exposing (..)

import AnyBag exposing (AnyBag)
import Config
import Data.Item exposing (Item)
import Data.Sound exposing (Sound)


type alias Minecart =
    { items : AnyBag String Item
    , movedFrom : Maybe ( Int, Int )
    }


emptyWagon : Minecart
emptyWagon =
    { items = AnyBag.empty Data.Item.toString
    , movedFrom = Nothing
    }


fullWagon : Item -> Minecart
fullWagon item =
    emptyWagon
        |> load
            ([ ( item, Config.wagonMaxItems ) ]
                |> AnyBag.fromAssociationList Data.Item.toString
            )


insert : Item -> Minecart -> ( Minecart, Sound )
insert item wagon =
    ( { wagon | items = AnyBag.insert 1 item wagon.items }
    , Data.Sound.PickUp
    )


moveFrom : ( Int, Int ) -> Minecart -> Minecart
moveFrom movedFrom wagon =
    { wagon | movedFrom = Just movedFrom }


stop : Minecart -> Minecart
stop wagon =
    { wagon | movedFrom = Nothing }


isFull : Minecart -> Bool
isFull wagon =
    AnyBag.size wagon.items >= Config.wagonMaxItems


load : AnyBag String Item -> Minecart -> Minecart
load items wagon =
    { wagon | items = items }


unload : Minecart -> Minecart
unload wagon =
    { wagon | items = AnyBag.empty Data.Item.toString }
