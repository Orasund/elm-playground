module Fish exposing (..)

import Config
import Dict
import Fish.Common exposing (BitColor(..), Fish)
import Random exposing (Generator)
import Rule exposing (Pattern(..))
import Set exposing (Set)
import WaveFunCollapse


type alias Random a =
    Generator a


generateDefault : Random Fish
generateDefault =
    let
        r a =
            [ Horizontal, Vertical, TopDown, BottomUp ]
                |> List.map (Tuple.pair a)
    in
    r True
        ++ r False
        |> generate


generate : List ( Bool, Pattern ) -> Random Fish
generate list =
    let
        rules =
            list
                |> List.map (\( b, p ) -> Rule.fromPattern p b)

        randPattern =
            Fish.Common.sprite
                |> Dict.filter (\_ -> identity)
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

        randPigment =
            Random.constant
                { blue = False
                , yellow = True
                , red = True
                }
    in
    Random.map3
        (\pattern primary secondary ->
            Fish.Common.defaultBreed
                |> (\breed ->
                        { breed
                            | pattern = Set.fromList pattern
                            , primary = primary
                            , secondary = secondary
                        }
                   )
                |> Fish.Common.new
        )
        randPattern
        randPigment
        randPigment


toBitmap : { animate : Bool } -> Set ( Int, Int ) -> List (List BitColor)
toBitmap args set =
    let
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
                            case Fish.Common.sprite |> Dict.get (permutate x y) of
                                Just False ->
                                    Black

                                Nothing ->
                                    None

                                Just True ->
                                    if Set.member (permutate x y) set then
                                        Primary

                                    else
                                        Secondary
                        )
            )
