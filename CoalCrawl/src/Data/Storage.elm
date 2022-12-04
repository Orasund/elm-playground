module Data.Storage exposing (..)

import AnyBag
import Data.Item exposing (Item)


type alias Storage =
    { items : List Item
    , maxAmount : Int
    }


empty : Int -> Storage
empty maxAmount =
    { items = []
    , maxAmount = maxAmount
    }


size : Storage -> Int
size storage =
    List.length storage.items


full : Int -> Item -> Storage
full maxAmount item =
    { items = List.repeat maxAmount item
    , maxAmount = maxAmount
    }


insert : Item -> Storage -> Maybe Storage
insert item storage =
    if isFull storage then
        Nothing

    else
        { storage
            | items = item :: storage.items
        }
            |> Just


isFull : Storage -> Bool
isFull storage =
    size storage == storage.maxAmount


isEmpty : Storage -> Bool
isEmpty storage =
    storage.items == []


load : List Item -> Storage -> Maybe Storage
load items storage =
    if List.length storage.items + List.length items <= storage.maxAmount then
        Just { storage | items = storage.items ++ items }

    else
        Nothing


unload : Storage -> ( Storage, List Item )
unload storage =
    ( { storage | items = [] }
    , storage.items
    )


toList : Storage -> List ( String, Int )
toList storage =
    storage.items
        |> AnyBag.fromList Data.Item.toString
        |> AnyBag.toAssociationList
