module Roguelike.GroupedList exposing (GroupedList, add, drop, fromList, reduce, rotateLeft, rotateRight, toList)


type alias GroupedList a =
    List ( a, Int )


drop : Int -> GroupedList a -> GroupedList a
drop n list =
    list |> List.drop n


fromList : List a -> GroupedList a
fromList list =
    list
        |> List.foldr
            (\elem list ->
                case list of
                    ( a, b ) :: c ->
                        if a == elem then
                            ( elem, b + 1 ) :: c
                        else
                            ( elem, 1 ) :: list

                    b ->
                        ( elem, 1 ) :: b
            )
            []


toList : GroupedList a -> List a
toList list =
    list
        |> List.concatMap
            (\( elem, num ) ->
                List.repeat num elem
            )


rotateLeft : GroupedList a -> GroupedList a
rotateLeft list =
    case list |> List.reverse of
        a :: b ->
            [ a ]
                |> List.append b
                |> List.reverse

        [] ->
            list


rotateRight : GroupedList a -> GroupedList a
rotateRight list =
    case list of
        a :: b ->
            [ a ] |> List.append b

        [] ->
            list


add : a -> GroupedList a -> GroupedList a
add target l =
    l
        |> List.foldr
            (\elem ( list, found ) ->
                if found == False then
                    if (elem |> Tuple.first) == target then
                        ( list
                            |> List.append [ ( target, (elem |> Tuple.second) + 1 ) ]
                        , True
                        )
                    else
                        ( list |> List.append [ elem ], False )
                else
                    ( list |> List.append [ elem ], True )
            )
            ( [], False )
        |> (\( list, found ) ->
                if found == False then
                    List.append list [ ( target, 1 ) ]
                else
                    list
           )


reduce : a -> GroupedList a -> GroupedList a
reduce target l =
    l
        |> List.foldr
            (\elem ( list, found ) ->
                if found == False then
                    if (elem |> Tuple.first) == target then
                        if (elem |> Tuple.second) - 1 <= 0 then
                            ( list, True )
                        else
                            ( list
                                |> List.append [ ( target, (elem |> Tuple.second) - 1 ) ]
                            , True
                            )
                    else
                        ( list |> List.append [ elem ], False )
                else
                    ( list |> List.append [ elem ], True )
            )
            ( [], False )
        |> Tuple.first
