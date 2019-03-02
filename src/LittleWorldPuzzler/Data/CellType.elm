module LittleWorldPuzzler.Data.CellType exposing
    ( CellType(..)
    , decoder
    , encode
    , list
    , toInt
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)


type CellType
    = Wood
    | Water
    | Fire
    | Stone
    | Volcano
    | Fog
    | Desert
    | Sea


list : List CellType
list =
    [ Sea, Desert, Fog, Volcano, Stone, Fire, Water, Wood ]


fromInt : Int -> CellType
fromInt n =
    case n of
        1 ->
            Wood

        2 ->
            Water

        3 ->
            Fire

        4 ->
            Stone

        5 ->
            Volcano

        6 ->
            Fog

        7 ->
            Desert

        8 ->
            Sea

        _ ->
            Wood


toInt : CellType -> Int
toInt cellType =
    case cellType of
        Wood ->
            1

        Water ->
            2

        Fire ->
            3

        Stone ->
            4

        Volcano ->
            5

        Fog ->
            6

        Desert ->
            7

        Sea ->
            8


toString : CellType -> String
toString cellType =
    String.fromChar <|
        case cellType of
            Wood ->
                '🌳'

            Water ->
                '💧'

            Fire ->
                '🔥'

            Stone ->
                '⛰'

            Volcano ->
                '🌋'

            Fog ->
                '☁'

            Desert ->
                '🏜'

            Sea ->
                '🌊'



{------------------------
   Decoder
------------------------}


decoder : Decoder CellType
decoder =
    D.int |> D.map fromInt



{------------------------
   Encoder
------------------------}


encode : CellType -> Value
encode =
    toInt >> E.int
