module AnyBag exposing (..)

import Bag exposing (Bag)


type alias AnyBag comparable a =
    { content : Bag comparable
    , encode : a -> comparable
    }


empty : (a -> comparable) -> AnyBag comparable a
empty encode =
    { content = Bag.empty, encode = encode }


insert : Int -> a -> AnyBag comparable a -> AnyBag comparable a
insert n a bag =
    { bag | content = Bag.insert n (bag.encode a) bag.content }


count : a -> AnyBag comparable a -> Int
count a bag =
    bag.content |> Bag.count (bag.encode a)


remove : Int -> a -> AnyBag comparable a -> AnyBag comparable a
remove n a bag =
    { bag | content = bag.content |> Bag.remove n (bag.encode a) }


toAssociationList : AnyBag comparable a -> List ( comparable, Int )
toAssociationList bag =
    bag.content |> Bag.toAssociationList


fromList : (a -> comparable) -> List a -> AnyBag comparable a
fromList encode list =
    { content = list |> List.map encode |> Bag.fromList, encode = encode }


{-| uses the encoding of the second bag
-}
union : AnyBag comparable a -> AnyBag comparable a -> AnyBag comparable a
union b1 b2 =
    { content = Bag.union b1.content b2.content
    , encode = b2.encode
    }


member : a -> AnyBag comparable a -> Bool
member a bag =
    bag.content |> Bag.member (bag.encode a)


size : AnyBag comparable a -> Int
size bag =
    Bag.size bag.content
