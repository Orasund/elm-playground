module Rule exposing (..)

import Array
import WaveFunCollapse exposing (Rule)


rule : List (List Int) -> Rule Int
rule list =
    let
        arr =
            list
                |> List.map
                    (\row ->
                        row
                            |> List.map
                                (\int ->
                                    if int == 0 then
                                        Just 0

                                    else if int == 1 then
                                        Just 1

                                    else
                                        Nothing
                                )
                            |> Array.fromList
                    )
                |> Array.fromList

        get ( x, y ) a =
            a
                |> Array.get y
                |> Maybe.andThen (Array.get x)
                |> Maybe.andThen identity
    in
    { neighbors =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( 0, 2 )
        , ( 1, 0 )
        , ( 1, 2 )
        , ( 2, 0 )
        , ( 2, 1 )
        , ( 2, 2 )
        ]
            |> List.filterMap
                (\p ->
                    arr
                        |> get p
                        |> Maybe.map (Tuple.pair (p |> Tuple.mapBoth ((+) -1) ((+) -1)))
                )
    , center = arr |> get ( 1, 1 ) |> Maybe.withDefault 0
    }


hor : Bool -> Rule Int
hor b =
    let
        t =
            if b then
                1

            else
                0
    in
    [ [ 9, 9, 9 ]
    , [ t, t, 9 ]
    , [ 9, 9, 9 ]
    ]
        |> rule


ver : Bool -> Rule Int
ver b =
    let
        t =
            if b then
                1

            else
                0
    in
    [ [ 9, t, 9 ]
    , [ 9, t, 9 ]
    , [ 9, 9, 9 ]
    ]
        |> rule


dia1 : Bool ->  (Rule Int)
dia1 b =
    let
        t =
            if b then
                1

            else
                0
    in
     [ [ t, 9, 9 ]
      , [ 9, t, 9 ]
      , [ 9, 9, 9 ]
      ]
        |> rule
    


dia2 : Bool -> Rule Int
dia2 b =
    let
        t =
            if b then
                1

            else
                0
    in
    [ [ 9, 9, t ]
    , [ 9, t, 9 ]
    , [ 9, 9, 9 ]
    ]
        |> rule
