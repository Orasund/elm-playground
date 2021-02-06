module StaticArray exposing (StaticArray, append10, append2, append20, append4, append5, append8, get, length, push, resize, singleton, toArray)

import Array exposing (Array)
import StaticArray.Index exposing (Index)
import StaticArray.Internal as Internal exposing (Index(..), Length(..), StaticArray(..))
import StaticArray.Label exposing (Eight, EightPlus, Five, FivePlus, Four, FourPlus, One, OnePlus(..), Ten, TenPlus, Twenty, TwentyPlus, Two, TwoPlus)
import StaticArray.Length exposing (Length)


type alias StaticArray n a =
    Internal.StaticArray n a


singleton : a -> StaticArray One a
singleton a =
    A ( a, Array.empty )


push : a -> StaticArray n a -> StaticArray (OnePlus n) a
push a (A ( head, tail )) =
    A ( head, Array.push a tail )


append2 : StaticArray Two a -> StaticArray n a -> StaticArray (TwoPlus n) a
append2 a1 (A ( h2, t2 )) =
    A ( h2, Array.append t2 (a1 |> toArray) )


append4 : StaticArray Four a -> StaticArray n a -> StaticArray (FourPlus n) a
append4 a1 (A ( h2, t2 )) =
    A ( h2, Array.append t2 (a1 |> toArray) )


append8 : StaticArray Eight a -> StaticArray n a -> StaticArray (EightPlus n) a
append8 a1 (A ( h2, t2 )) =
    A ( h2, Array.append t2 (a1 |> toArray) )


append5 : StaticArray Five a -> StaticArray n a -> StaticArray (FivePlus n) a
append5 a1 (A ( h2, t2 )) =
    A ( h2, Array.append t2 (a1 |> toArray) )


append10 : StaticArray Ten a -> StaticArray n a -> StaticArray (TenPlus n) a
append10 a1 (A ( h2, t2 )) =
    A ( h2, Array.append t2 (a1 |> toArray) )


append20 : StaticArray Twenty a -> StaticArray n a -> StaticArray (TwentyPlus n) a
append20 a1 (A ( h2, t2 )) =
    A ( h2, Array.append t2 (a1 |> toArray) )


resize : Length m -> StaticArray n a -> StaticArray m a
resize (C l) (A ( head, tail )) =
    let
        diff =
            (tail |> Array.length |> (+) 1) - l
    in
    if diff < 0 then
        A ( head, Array.append tail (Array.repeat diff head) )

    else
        A ( head, tail |> Array.slice 0 l )


toArray : StaticArray n a -> Array a
toArray (A ( head, tail )) =
    Array.append ([ head ] |> Array.fromList) tail


get : Index n -> StaticArray n a -> a
get (I n) (A ( head, tail )) =
    if n == 0 then
        head

    else
        tail
            |> Array.get (n - 1)
            |> Maybe.withDefault head


length : StaticArray n a -> Length n
length (A ( _, tail )) =
    tail |> Array.length |> (+) 1 |> C
