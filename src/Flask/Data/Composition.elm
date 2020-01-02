module Flask.Data.Composition exposing (Composition, empty, fromList, insert, remove, toList, toString)

import Flask.Data.Base as Base exposing (Base(..))


type alias Composition =
    { y1 : Int
    , y2 : Bool
    , r1 : Int
    , r2 : Int
    , g1 : Int
    , g2 : Bool
    , b1 : Int
    , b2 : Int
    }


empty : Composition
empty =
    { y1 = 0
    , y2 = False
    , r1 = 0
    , r2 = 0
    , g1 = 0
    , g2 = False
    , b1 = 0
    , b2 = 0
    }


insert : Base -> Composition -> Composition
insert base out =
    case base of
        Y1 ->
            { out | y1 = min 3 <| out.y1 + 1 }

        Y2 ->
            { out | y2 = True }

        R1 ->
            { out | r1 = min 3 <| out.r1 + 1 }

        R2 ->
            { out | r2 = min 3 <| out.r2 + 1 }

        G1 ->
            { out | g1 = min 3 <| out.g1 + 1 }

        G2 ->
            { out | g2 = True }

        B1 ->
            { out | b1 = min 3 <| out.b1 + 1 }

        B2 ->
            { out | b2 = min 3 <| out.b2 + 1 }


remove : Base -> Composition -> Composition
remove base out =
    case base of
        Y1 ->
            { out | y1 = max 0 <| out.y1 - 1 }

        Y2 ->
            { out | y2 = False }

        R1 ->
            { out | r1 = max 0 <| out.r1 - 1 }

        R2 ->
            { out | r2 = max 0 <| out.r2 - 1 }

        G1 ->
            { out | g1 = max 0 <| out.g1 - 1 }

        G2 ->
            { out | g2 = False }

        B1 ->
            { out | b1 = max 0 <| out.b1 - 1 }

        B2 ->
            { out | b2 = max 0 <| out.b2 - 1 }


fromList : List Base -> Composition
fromList =
    List.foldl insert empty


toList : Composition -> List Base
toList { b1, b2, g1, g2, r1, r2, y1, y2 } =
    List.concat
        [ if g2 then
            [ G2 ]

          else
            []
        , Y1 |> List.repeat y1
        , B1 |> List.repeat b1
        , B2 |> List.repeat b2
        , G1 |> List.repeat g1
        , R1 |> List.repeat r1
        , R2 |> List.repeat r2
        , if y2 then
            [ Y2 ]

          else
            []
        ]


toString : Composition -> String
toString =
    toList
        >> List.map Base.toString
        >> List.intersperse " - "
        >> String.concat
