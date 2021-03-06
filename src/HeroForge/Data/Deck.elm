module HeroForge.Data.Deck exposing (Deck, add, addPrevious, current, findNext, findPrevious, fromList, next, remove, toList)

import RollingList exposing (RollingList)


type alias Deck a =
    RollingList a


findNext : (a -> Bool) -> Deck a -> Deck a
findNext cond deck =
    case deck |> RollingList.current of
        Just card ->
            if cond card then
                deck

            else
                deck
                    |> RollingList.roll
                    |> findNext cond

        Nothing ->
            deck


findPrevious : (a -> Bool) -> Deck a -> Deck a
findPrevious cond deck =
    case deck |> RollingList.current of
        Just card ->
            if cond card then
                deck

            else
                deck
                    |> RollingList.rollBack
                    |> findNext cond

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
