module Katakomben.Data.Deck exposing (Deck, add, addPrevious, current, fromList, jumpTo, next, remove, toList)

import RollingList exposing (RollingList)


type alias Deck a =
    RollingList a


jumpTo : a -> Deck a -> Deck a
jumpTo target deck =
    case deck |> RollingList.current of
        Just card ->
            if card == target then
                deck

            else
                deck
                    |> RollingList.roll
                    |> jumpTo target

        Nothing ->
            deck


fromList : List a -> Deck a
fromList =
    RollingList.fromList


toList : Deck a -> List a
toList =
    RollingList.toList


addPrevious : a -> Deck a -> Deck a
addPrevious a deck =
    deck
        |> add a
        |> RollingList.roll


add : a -> Deck a -> Deck a
add a deck =
    (case deck |> RollingList.toList of
        b :: list ->
            a :: b :: list

        [] ->
            [ a ]
    )
        |> RollingList.fromList


next : Deck a -> Deck a
next =
    RollingList.roll


remove : Deck a -> Deck a
remove deck =
    deck
        |> RollingList.toList
        |> List.tail
        |> Maybe.map RollingList.fromList
        |> Maybe.withDefault deck


current : Deck a -> Maybe a
current =
    RollingList.current
