module World exposing (..)

import Config
import Dict exposing (Dict)
import Random exposing (Generator)
import Tile exposing (Tile)


generate : Int -> Generator (Dict Int Tile)
generate level =
    Random.map2
        (\rightSide leftSide ->
            [ leftSide
                |> List.reverse
            , [ Just Tile.Player ]
            , rightSide
            ]
                |> List.concat
                |> List.indexedMap
                    (\i tile ->
                        ( i - Config.maxDistance, tile )
                    )
                |> List.filterMap
                    (\( i, tile ) ->
                        tile |> Maybe.map (Tuple.pair i)
                    )
                |> Dict.fromList
        )
        (generateSide level)
        (generateSide level)


generateSide : Int -> Generator (List (Maybe Tile))
generateSide level =
    let
        maxAmounts =
            (case level of
                1 ->
                    [ ( Tile.Shield, 0 )
                    , ( Tile.AxeThrower, 0 )
                    , ( Tile.Chair, 0 )
                    , ( Tile.Money, 0 )
                    , ( Tile.Enemy, 1 )
                    ]

                2 ->
                    [ ( Tile.Shield, 0 )
                    , ( Tile.AxeThrower, 0 )
                    , ( Tile.Money, 0 )
                    , ( Tile.Chair, 1 )
                    , ( Tile.Enemy, 2 )
                    ]

                3 ->
                    [ ( Tile.Shield, 0 )
                    , ( Tile.AxeThrower, 0 )
                    , ( Tile.Money, 0 )
                    , ( Tile.Chair, 2 )
                    , ( Tile.Enemy, 3 )
                    ]

                4 ->
                    [ ( Tile.Shield, 0 )
                    , ( Tile.AxeThrower, 1 )
                    , ( Tile.Money, 2 )
                    , ( Tile.Chair, 0 )
                    , ( Tile.Enemy, 0 )
                    ]

                5 ->
                    [ ( Tile.Shield, 0 )
                    , ( Tile.AxeThrower, 1 )
                    , ( Tile.Money, 1 )
                    , ( Tile.Chair, 2 )
                    , ( Tile.Enemy, 2 )
                    ]

                6 ->
                    [ ( Tile.Shield, 0 )
                    , ( Tile.AxeThrower, 1 )
                    , ( Tile.Money, 1 )
                    , ( Tile.Chair, 2 )
                    , ( Tile.Enemy, 3 )
                    ]

                7 ->
                    [ ( Tile.Shield, 1 )
                    , ( Tile.AxeThrower, 2 )
                    , ( Tile.Money, 1 )
                    , ( Tile.Chair, 2 )
                    , ( Tile.Enemy, 1 )
                    ]

                8 ->
                    [ ( Tile.Shield, 1 )
                    , ( Tile.AxeThrower, 2 )
                    , ( Tile.Money, 1 )
                    , ( Tile.Chair, 2 )
                    , ( Tile.Enemy, 1 )
                    ]

                9 ->
                    [ ( Tile.Shield, 2 )
                    , ( Tile.AxeThrower, 3 )
                    , ( Tile.Money, 1 )
                    , ( Tile.Chair, 2 )
                    , ( Tile.Enemy, 2 )
                    ]

                _ ->
                    [ ( Tile.Enemy, 2 ) ]
            )
                |> List.map (Tuple.mapFirst Tile.emoji)
                |> Dict.fromList
    in
    List.repeat (Config.maxDistance - 1) ()
        |> List.foldl
            (\() ->
                Random.andThen
                    (\{ lastTile, amounts, result } ->
                        (maxAmounts
                            |> Dict.filter
                                (\k maxAmount ->
                                    amounts
                                        |> Dict.get k
                                        |> Maybe.map (\amount -> amount)
                                        |> Maybe.withDefault 0
                                        |> (\it -> it >= maxAmount)
                                )
                            |> Dict.keys
                        )
                            |> nextTile lastTile
                            |> Random.map
                                (\t ->
                                    { lastTile = t
                                    , amounts =
                                        amounts
                                            |> Dict.update
                                                (t
                                                    |> Maybe.map Tile.emoji
                                                    |> Maybe.withDefault ""
                                                )
                                                (\maybe ->
                                                    maybe
                                                        |> Maybe.map ((+) 1)
                                                        |> Maybe.withDefault 1
                                                        |> Just
                                                )
                                    , result = t :: result
                                    }
                                )
                    )
            )
            ({ lastTile = Just Tile.Player
             , amounts = Dict.empty
             , result = []
             }
                |> Random.constant
            )
        |> Random.map
            (\{ result } ->
                Just Tile.Door
                    :: result
                    |> List.reverse
            )


nextTile : Maybe Tile -> List String -> Generator (Maybe Tile)
nextTile tile blackList =
    (case tile of
        Just Tile.Player ->
            [ ( 2, Tile.Chair )
            , ( 1, Tile.Shield )
            ]

        Just Tile.Enemy ->
            [ ( 1, Tile.AxeThrower )
            , ( 1, Tile.Enemy )
            , ( 1, Tile.Chair )
            ]

        Just Tile.Chair ->
            [ ( 2, Tile.AxeThrower )
            , ( 1, Tile.Enemy )
            ]

        Just Tile.Axe ->
            [ ( 1, Tile.AxeThrower ) ]

        Just Tile.AxeThrower ->
            [ ( 2, Tile.Enemy )
            , ( 2, Tile.Shield )
            , ( 1, Tile.Money )
            ]

        Just Tile.Shield ->
            [ ( 1, Tile.Chair ) ]

        Just Tile.Door ->
            []

        Just Tile.Money ->
            [ ( 1, Tile.Money )
            , ( 1, Tile.Chair )
            ]

        Nothing ->
            [ ( 2, Tile.AxeThrower )
            , ( 1, Tile.Enemy )
            ]
    )
        |> List.filterMap
            (\( prop, t ) ->
                if blackList |> List.member (Tile.emoji t) then
                    Nothing

                else
                    Just ( prop, Just t )
            )
        |> Random.weighted ( 0, Nothing )
