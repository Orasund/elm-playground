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


list : List CellType
list =
    [ Wood, Water, Fire, Stone, Volcano, Fog, Desert ]


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


toString : CellType -> String
toString cellType =
    String.fromChar <|
        case cellType of
            Wood ->
                '🌳'

            Water ->
                '🌊'

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
