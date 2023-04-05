module Fish exposing (..)

import Config
import Dict
import Random exposing (Generator)
import Rule
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
    , [ 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 0 ]
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


generatePattern : List (Rule Int) -> Generator (List ( Int, Int ))
generatePattern rules =
    sprite
        |> Dict.filter
            (\_ value ->
                value /= 1
            )
        |> Dict.keys
        |> WaveFunCollapse.generator rules
        |> Random.map
            (\maybe ->
                maybe
                    |> Maybe.map
                        (\dict ->
                            dict
                                |> Dict.filter (\_ int -> int /= 0)
                                |> Dict.keys
                        )
                    |> Maybe.withDefault []
            )


withPattern : { animate : Bool } -> List ( Int, Int ) -> List (List BitColor)
withPattern args pattern =
    let
        set =
            Set.fromList pattern

        permutate x y =
            if x < 5 && args.animate then
                ( x, y + x // 2 - 3 |> modBy Config.spriteSize )

            else
                ( x, y )
    in
    List.repeat Config.spriteSize ()
        |> List.indexedMap
            (\y () ->
                List.repeat Config.spriteSize ()
                    |> List.indexedMap
                        (\x () ->
                            case sprite |> Dict.get (permutate x y) of
                                Just 1 ->
                                    Black

                                Nothing ->
                                    None

                                Just _ ->
                                    if Set.member (permutate x y) set then
                                        Primary

                                    else
                                        Secondary
                        )
            )
