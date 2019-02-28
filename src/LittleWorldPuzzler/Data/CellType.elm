module LittleWorldPuzzler.Data.CellType exposing (CellType(..), list, toInt, toString)


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
