module Fish exposing (..)

import Color exposing (Color)
import Config
import Dict exposing (Dict)
import Html exposing (p)
import Random exposing (Generator)
import Rule exposing (Pattern(..))
import Set exposing (Set)
import WaveFunCollapse


type alias Random a =
    Generator a


type alias FishId =
    Int


type alias Fish =
    { pattern : Set ( Int, Int )
    , rules : List ( Bool, Pattern )
    , primary : Color
    , secondary : Color
    }


type BitColor
    = None
    | Black
    | Primary
    | Secondary


sprite : Dict ( Int, Int ) Bool
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
        |> Dict.map
            (\_ int ->
                case int of
                    1 ->
                        False

                    _ ->
                        True
            )


fromPattern : List ( Int, Int ) -> Fish
fromPattern p =
    let
        computedRules =
            computeRules (Set.fromList p)

        ( secondaryRuleAmount, primaryRuleAmount ) =
            computedRules
                |> List.partition Tuple.first
                |> Tuple.mapBoth List.length List.length
    in
    { pattern = p |> Set.fromList
    , rules = computedRules
    , primary =
        case primaryRuleAmount of
            1 ->
                Color.lightGray

            2 ->
                Color.lightBlue

            3 ->
                Color.blue

            _ ->
                Color.darkBlue
    , secondary =
        case secondaryRuleAmount of
            1 ->
                Color.darkGray

            2 ->
                Color.lightRed

            3 ->
                Color.red

            _ ->
                Color.darkRed
    }


generate : List ( Bool, Pattern ) -> Random Fish
generate list =
    let
        rules =
            list
                |> List.map (\( b, p ) -> Rule.fromPattern p b)
    in
    sprite
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
        |> Random.map fromPattern


computeRules : Set ( Int, Int ) -> List ( Bool, Pattern )
computeRules set =
    let
        dict =
            sprite
                |> Dict.filter (\_ -> identity)
                |> Dict.map (\p _ -> Set.member p set |> not)

        findMostUsed list =
            list
                |> List.foldl
                    (\string ->
                        Dict.update string
                            (\maybe ->
                                maybe
                                    |> Maybe.map ((+) 1)
                                    |> Maybe.withDefault 0
                                    |> Just
                            )
                    )
                    Dict.empty
                |> Dict.toList
                |> List.sortBy Tuple.second
                |> List.reverse
                |> List.head
                |> Maybe.map Tuple.first

        toBool : Int -> Bool
        toBool int =
            int /= 0

        fromBool : Bool -> Int
        fromBool bool =
            if bool then
                1

            else
                0

        lookupDict =
            dict
                |> Dict.map (\_ -> fromBool)

        rec d output =
            d
                |> Dict.toList
                |> List.concatMap
                    (\( pos, b ) ->
                        [ Horizontal, Vertical, BottomUp, TopDown ]
                            |> List.filter
                                (\p ->
                                    WaveFunCollapse.isValidRule pos
                                        lookupDict
                                        (Rule.fromPattern p b)
                                )
                            |> List.map Rule.toString
                            |> List.map (Tuple.pair (fromBool b))
                    )
                |> findMostUsed
                |> Maybe.andThen (\( b, r ) -> r |> Rule.fromString |> Maybe.map (Tuple.pair b))
                |> (\maybeRule ->
                        case maybeRule of
                            Just ( b, pattern ) ->
                                let
                                    rule =
                                        Rule.fromPattern pattern (toBool b)
                                in
                                rec
                                    (d
                                        |> Dict.filter
                                            (\pos b0 ->
                                                (b0 /= toBool b)
                                                    || (WaveFunCollapse.isValidRule pos
                                                            lookupDict
                                                            rule
                                                            |> not
                                                       )
                                            )
                                    )
                                    (( toBool b, pattern ) :: output)

                            Nothing ->
                                output
                   )
    in
    rec dict []


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
                            case sprite |> Dict.get (permutate x y) of
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


fromParents : Fish -> Fish -> Random Fish
fromParents fish1 fish2 =
    let
        rules =
            (fish1.rules ++ fish2.rules)
                |> List.map (\( b, p ) -> Rule.fromPattern p b)

        positions =
            sprite
                |> Dict.filter (\_ -> identity)
                |> Dict.keys

        commonPositions =
            positions
                |> List.filterMap
                    (\pos ->
                        if Set.member pos fish1.pattern && Set.member pos fish2.pattern then
                            Just ( pos, 1 )

                        else if not (Set.member pos fish1.pattern) && not (Set.member pos fish2.pattern) then
                            Just ( pos, 0 )

                        else
                            Nothing
                    )

        randOutput =
            Random.float 0 1
                |> Random.list (List.length commonPositions)
                |> Random.map
                    (\randList ->
                        List.map2 Tuple.pair
                            commonPositions
                            randList
                            |> List.filterMap
                                (\( p, r ) ->
                                    if r < 0.95 then
                                        Just p

                                    else
                                        Nothing
                                )
                    )
                |> Random.map Dict.fromList
    in
    randOutput
        |> Random.map
            (\output ->
                positions
                    |> WaveFunCollapse.new rules
                    |> WaveFunCollapse.withOutput output
            )
        |> Random.map WaveFunCollapse.checkRemaining
        |> Random.andThen WaveFunCollapse.build
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
        |> Random.map fromPattern
