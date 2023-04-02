module FishPattern exposing (..)

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


horizontal : List (Rule Int)
horizontal =
    [ [ [ 9, 0, 9 ]
      , [ 1, 1, 1 ]
      , [ 9, 0, 9 ]
      ]
        |> rule
    , [ [ 9, 1, 9 ]
      , [ 0, 0, 0 ]
      , [ 9, 1, 9 ]
      ]
        |> rule
    ]


vertical : List (Rule Int)
vertical =
    [ [ [ 9, 1, 9 ]
      , [ 0, 1, 0 ]
      , [ 9, 1, 9 ]
      ]
        |> rule
    , [ [ 9, 0, 9 ]
      , [ 1, 0, 1 ]
      , [ 9, 0, 9 ]
      ]
        |> rule
    ]


diagonal1 : List (Rule Int)
diagonal1 =
    [ [ [ 9, 0, 9 ]
      , [ 1, 0, 0 ]
      , [ 9, 1, 9 ]
      ]
        |> rule
    , [ [ 9, 0, 9 ]
      , [ 1, 1, 0 ]
      , [ 9, 1, 9 ]
      ]
        |> rule
    , [ [ 9, 1, 9 ]
      , [ 0, 0, 1 ]
      , [ 9, 0, 9 ]
      ]
        |> rule
    , [ [ 9, 1, 9 ]
      , [ 0, 1, 1 ]
      , [ 9, 0, 9 ]
      ]
        |> rule
    ]


diagonal2 : List (Rule Int)
diagonal2 =
    [ [ [ 9, 1, 9 ]
      , [ 1, 1, 0 ]
      , [ 9, 0, 9 ]
      ]
        |> rule
    , [ [ 9, 0, 9 ]
      , [ 0, 0, 1 ]
      , [ 9, 1, 9 ]
      ]
        |> rule
    , [ [ 9, 0, 9 ]
      , [ 0, 1, 1 ]
      , [ 9, 1, 9 ]
      ]
        |> rule
    , [ [ 9, 1, 9 ]
      , [ 1, 0, 0 ]
      , [ 9, 0, 9 ]
      ]
        |> rule
    ]


dots : List (Rule Int)
dots =
    [ [ [ 9, 0, 9 ]
      , [ 0, 1, 0 ]
      , [ 9, 0, 9 ]
      ]
        |> rule
    , [ [ 9, 1, 9 ]
      , [ 1, 0, 1 ]
      , [ 9, 1, 9 ]
      ]
        |> rule
    ]
