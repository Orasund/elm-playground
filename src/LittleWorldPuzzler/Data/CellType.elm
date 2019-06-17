module LittleWorldPuzzler.Data.CellType exposing
    ( CellType(..)
    , json
    , list
    , toInt
    , toString
    )

import Jsonstore exposing (Json)


type CellType
    = Wood
    | Water
    | Fire
    | Stone
    | Volcano
    | Fog
    | Desert
    | Glacier
    | Ice
    | Snow
    | Evergreen


old_List : List CellType
old_List =
    [ Desert, Fire, Glacier, Volcano, Stone, Ice, Fog, Water, Wood ]


list : List CellType
list =
    [ Snow, Desert, Fire, Glacier, Volcano, Stone, Evergreen, Ice, Fog, Water, Wood ]


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
            Glacier

        9 ->
            Ice

        10 ->
            Snow

        11 ->
            Evergreen

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

        Glacier ->
            8

        Ice ->
            9

        Snow ->
            10

        Evergreen ->
            11


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

            Glacier ->
                '🏔'

            Ice ->
                '❄'

            Snow ->
                '⛄'

            Evergreen ->
                '🌲'



{- Bug ->
   '🐞'
-}
{------------------------
   Json
------------------------}


json : Json CellType
json =
    Jsonstore.int
        |> Jsonstore.map
            fromInt
            toInt
