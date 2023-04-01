module Fish exposing (..)

import Config
import Dict
import FishPattern
import Random exposing (Generator)
import Set
import WaveFunCollapse exposing (Rule)


type BitColor
    = None
    | Black
    | Primary
    | Secondary


sprite =
    [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0 ]
    , [ 1, 2, 2, 2, 1, 0, 1, 1, 2, 2, 2, 2, 2, 1, 0, 0 ]
    , [ 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 0 ]
    , [ 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1 ]
    , [ 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1 ]
    , [ 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 0 ]
    , [ 1, 2, 2, 2, 1, 0, 1, 1, 2, 2, 2, 2, 2, 1, 0, 0 ]
    , [ 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    ]
        |> List.indexedMap
            (\y list ->
                list
                    |> List.indexedMap
                        (\x value ->
                            ( ( x, y ), value )
                        )
                    |> List.filter
                        (\( _, value ) ->
                            value /= 0
                        )
            )
        |> List.concat
        |> Dict.fromList


generatePattern : List (Rule Bool) -> Generator (List ( Int, Int ))
generatePattern rules =
    sprite
        |> Dict.filter
            (\_ value ->
                value /= 1
            )
        |> Dict.keys
        |> WaveFunCollapse.generator rules
        |> Random.map
            (\dict ->
                dict
                    |> Dict.filter (\_ -> identity)
                    |> Dict.keys
            )


withPattern : List ( Int, Int ) -> List (List BitColor)
withPattern pattern =
    let
        set =
            Set.fromList pattern
    in
    List.repeat Config.spriteSize ()
        |> List.indexedMap
            (\y () ->
                List.repeat Config.spriteSize ()
                    |> List.indexedMap
                        (\x () ->
                            case sprite |> Dict.get ( x, y ) of
                                Just 1 ->
                                    Black

                                Nothing ->
                                    None

                                Just _ ->
                                    if Set.member ( x, y ) set then
                                        Primary

                                    else
                                        Secondary
                        )
            )
