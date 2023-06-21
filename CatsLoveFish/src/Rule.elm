module Rule exposing (..)

import WaveFunCollapse exposing (Rule)


type Pattern
    = Horizontal
    | Vertical
    | BottomUp
    | TopDown


toString : Pattern -> String
toString pattern =
    case pattern of
        Horizontal ->
            "Horizontal"

        Vertical ->
            "Vertical"

        BottomUp ->
            "BottomUp"

        TopDown ->
            "TopDown"


fromString : String -> Maybe Pattern
fromString string =
    case string of
        "Horizontal" ->
            Just Horizontal

        "Vertical" ->
            Just Vertical

        "BottomUp" ->
            Just BottomUp

        "TopDown" ->
            Just TopDown

        _ ->
            Nothing


fromPattern : Pattern -> Bool -> Rule Int
fromPattern pattern bool =
    let
        t =
            if bool then
                1

            else
                0
    in
    { neighbors =
        case pattern of
            Horizontal ->
                [ ( ( -1, 0 ), t ) ]

            Vertical ->
                [ ( ( 0, -1 ), t ) ]

            BottomUp ->
                [ ( ( 1, -1 ), t ) ]

            TopDown ->
                [ ( ( -1, -1 ), t ) ]
    , center = t
    }
