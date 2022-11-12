module Data.Wagon exposing (..)

import AnyBag exposing (AnyBag)
import Config
import Data.Item exposing (Item)


type alias Wagon =
    { items : AnyBag String Item
    , movedFrom : Maybe ( Int, Int )
    }


emptyWagon : Wagon
emptyWagon =
    { items = AnyBag.empty Data.Item.toString
    , movedFrom = Nothing
    }


insert : Item -> Wagon -> Wagon
insert item wagon =
    { wagon | items = AnyBag.insert 1 item wagon.items }


moveFrom : ( Int, Int ) -> Wagon -> Wagon
moveFrom movedFrom wagon =
    { wagon | movedFrom = Just movedFrom }


isFull : Wagon -> Bool
isFull wagon =
    AnyBag.size wagon.items >= Config.wagonMaxItems


load : AnyBag String Item -> Wagon -> Wagon
load items wagon =
    { wagon | items = items }


unload : Wagon -> Wagon
unload wagon =
    { wagon | items = AnyBag.empty Data.Item.toString }
