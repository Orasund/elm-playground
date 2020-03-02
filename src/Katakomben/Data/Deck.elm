module Katakomben.Data.Deck exposing (Deck, addNext,toList, addPrevious, current, fromList, jumpTo, next, remove)

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

addNext : a -> Deck a -> Deck a
addNext a deck =
    (case deck |> RollingList.toList of
        b :: list ->
            b :: a :: list

        [] ->
            [ a ]
    )
        |> RollingList.fromList


addPrevious : a -> Deck a -> Deck a
addPrevious a deck =
    (case deck |> RollingList.toList of
        b :: list ->
            a :: b :: list

        [] ->
            [ a ]
    )
        |> RollingList.fromList
        |> RollingList.roll


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
