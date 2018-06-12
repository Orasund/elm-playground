module Roguelike.Inventory exposing (Inventory, add, drop, get, ground, init, rotateLeft, rotateRight, selected, take)

import GroupedList exposing (GroupedList)
import OverflowList exposing (OverflowList)


type alias Inventory a =
    OverflowList a


init : Int -> Inventory a
init max_length =
    OverflowList.fromList max_length []


add : a -> Inventory a -> Inventory a
add item inventory =
    inventory
        |> OverflowList.getList
        |> (\list ->
                if list |> List.length |> (>) (inventory |> OverflowList.getMaxLength) then
                    list
                        |> GroupedList.fromList
                        |> GroupedList.add item
                        |> GroupedList.toList
                else
                    List.append list [ item ]
           )
        |> OverflowList.fromList (inventory |> OverflowList.getMaxLength)


rotateLeft : Inventory a -> Inventory a
rotateLeft inventory =
    inventory
        |> OverflowList.getOverflow
        |> Maybe.withDefault []
        |> List.append
            (inventory
                |> OverflowList.getList
                |> GroupedList.fromList
                |> GroupedList.rotateLeft
                |> GroupedList.toList
            )
        |> OverflowList.fromList (inventory |> OverflowList.getMaxLength)


rotateRight : Inventory a -> Inventory a
rotateRight inventory =
    inventory
        |> OverflowList.getOverflow
        |> Maybe.withDefault []
        |> List.append
            (inventory
                |> OverflowList.getList
                |> GroupedList.fromList
                |> GroupedList.rotateRight
                |> GroupedList.toList
            )
        |> OverflowList.fromList (inventory |> OverflowList.getMaxLength)


drop : Inventory a -> ( Maybe a, Inventory a )
drop inventory =
    ( case inventory |> OverflowList.getOverflow of
        Just a ->
            a |> List.head

        Nothing ->
            Nothing
    , inventory
        |> OverflowList.getList
        |> OverflowList.fromList (inventory |> OverflowList.getMaxLength)
    )


take : Inventory a -> ( Maybe a, Inventory a )
take inventory =
    ( inventory |> OverflowList.getList |> List.head
    , inventory
        |> OverflowList.getList
        |> List.drop 1
        |> (\list ->
                case inventory |> OverflowList.getOverflow of
                    Just a ->
                        list |> List.append a

                    Nothing ->
                        list
           )
        |> OverflowList.fromList (inventory |> OverflowList.getMaxLength)
    )


get : Inventory a -> List a
get inventory =
    inventory |> OverflowList.getList


ground : Inventory a -> Maybe a
ground inventory =
    case inventory |> OverflowList.getOverflow of
        Just (a :: _) ->
            Just a

        _ ->
            Nothing


selected : Inventory a -> Maybe a
selected inventory =
    case inventory |> OverflowList.getList of
        a :: _ ->
            Just a

        _ ->
            Nothing
