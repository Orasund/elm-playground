module ListBag exposing (..)


empty : List ( a, Int )
empty =
    []


singleton : a -> List ( a, Int )
singleton a =
    [ ( a, 1 ) ]


isEmpty : List ( a, Int ) -> Bool
isEmpty =
    List.isEmpty


insert : Int -> a -> List ( a, Int ) -> List ( a, Int )
insert n a list =
    let
        rec input output =
            case input of
                ( a1, n1 ) :: tail ->
                    if a1 == a then
                        ( a1, n1 + n ) :: tail ++ output

                    else
                        rec tail (( a1, n1 ) :: output)

                [] ->
                    ( a, n ) :: output
    in
    rec list []


count : a -> List ( a, Int ) -> Int
count a list =
    let
        rec input =
            case input of
                ( a1, n1 ) :: tail ->
                    if a1 == a then
                        n1

                    else
                        rec tail

                [] ->
                    0
    in
    rec list


remove : Int -> a -> List ( a, Int ) -> List ( a, Int )
remove n a list =
    let
        rec input output =
            case input of
                ( a1, n1 ) :: tail ->
                    if a == a1 then
                        if n1 > n then
                            ( a1, n1 - n ) :: tail ++ output

                        else
                            tail ++ output

                    else
                        rec tail (( a1, n1 ) :: output)

                [] ->
                    output
    in
    rec list []


toList : List ( a, Int ) -> List a
toList =
    List.concatMap (\( a, n ) -> List.repeat n a)


fromList : List a -> List ( a, Int )
fromList =
    List.foldl (\a -> insert 1 a) []


{-| uses the encoding of the second bag
-}
union : List ( a, Int ) -> List ( a, Int ) -> List ( a, Int )
union l1 =
    List.foldl (\( a, n ) -> insert n a) l1


member : a -> List ( a, Int ) -> Bool
member a =
    List.any (\( a1, _ ) -> a == a1)


size : List ( a, Int ) -> Int
size list =
    list |> List.map Tuple.second |> List.sum
