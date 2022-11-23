module Data.Storage exposing (..)

import AnyBag exposing (AnyBag)
import Data.Item exposing (Item)


type alias Storage =
    { items : AnyBag String Item
    , maxAmount : Int
    }


empty : Int -> Storage
empty maxAmount =
    { items = AnyBag.empty Data.Item.toString
    , maxAmount = maxAmount
    }


size : Storage -> Int
size storage =
    AnyBag.size storage.items


full : Int -> Item -> Storage
full maxAmount item =
    { items = [ ( item, maxAmount ) ] |> AnyBag.fromAssociationList Data.Item.toString
    , maxAmount = maxAmount
    }


insert : Item -> Storage -> Maybe Storage
insert item storage =
    if isFull storage then
        Nothing

    else
        { storage
            | items =
                storage.items
                    |> AnyBag.insert 1 item
        }
            |> Just


isFull : Storage -> Bool
isFull storage =
    AnyBag.size storage.items == storage.maxAmount


isEmpty : Storage -> Bool
isEmpty storage =
    AnyBag.isEmpty storage.items


load : AnyBag String Item -> Storage -> Maybe Storage
load items storage =
    if AnyBag.size items + AnyBag.size storage.items <= storage.maxAmount then
        Just { storage | items = storage.items |> AnyBag.union items }

    else
        Nothing


unload : Storage -> ( Storage, AnyBag String Item )
unload storage =
    ( { storage | items = AnyBag.empty Data.Item.toString }
    , storage.items
    )


toList : Storage -> List ( String, Int )
toList storage =
    storage.items
        |> AnyBag.toAssociationList
