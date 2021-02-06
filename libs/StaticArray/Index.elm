module StaticArray.Index exposing (Index, decrease, first, fromLessThen, fromModBy, increase, last, setLength, toInt)

import StaticArray.Internal as Internal exposing (Index(..), Length(..))


type alias Index n =
    Internal.Index n


first : Index n
first =
    I 0


last : Length n -> Index n
last (C const) =
    I const


fromModBy : Length n -> Int -> Index n
fromModBy (C const) =
    modBy const >> I


fromLessThen : Length n -> Int -> Index n
fromLessThen (C const) =
    min (const - 1) >> I


increase : Length n -> Index n -> Maybe (Index n)
increase (C const) (I n) =
    if n + 1 < const then
        Just <| I (n + 1)

    else
        Nothing


decrease : Index n -> Maybe (Index n)
decrease (I n) =
    if n == 0 then
        Nothing

    else
        Just <| I (n - 1)


toInt : Index n -> Int
toInt (I n) =
    n


setLength : Length m -> Index n -> Maybe (Index m)
setLength (C const) (I n) =
    if n <= const then
        Just (I n)

    else
        Nothing
