-- List with overflow


module OverflowList exposing (OverflowList, fromList, getList, getMaxLength, getOverflow, length, overflow)


type alias OverflowList a =
    { list : List a
    , max_length : Int
    }


fromList : Int -> List a -> OverflowList a
fromList max_length list =
    { list = list, max_length = max_length }


getList : OverflowList a -> List a
getList overflowList =
    overflowList.list
        |> List.take overflowList.max_length


length : OverflowList a -> Int
length overflowList =
    overflowList.list
        |> List.length
        |> min overflowList.max_length


getMaxLength : OverflowList a -> Int
getMaxLength overflowList =
    overflowList.max_length


overflow : OverflowList a -> Int
overflow overflowList =
    overflowList.list
        |> List.length
        |> (-) overflowList.max_length
        |> max 0


getOverflow : OverflowList a -> Maybe (List a)
getOverflow overflowList =
    overflowList.list
        |> List.drop overflowList.max_length
        |> (\list ->
                if list |> List.isEmpty then
                    Nothing
                else
                    Just list
           )


append : List a -> OverflowList a -> OverflowList a
append list overflowList =
    { overflowList
        | list = list |> List.append overflowList.list
    }
