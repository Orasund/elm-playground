module Fish.Common exposing (..)

import Dict exposing (Dict)
import Pigment exposing (Pigment)
import Random exposing (Generator)
import Rule exposing (Pattern(..))
import Set exposing (Set)
import WaveFunCollapse


type alias FishId =
    Int


type alias BreedId =
    ( ( Int, Int, Int ), ( Int, Int, Int ) )


type alias Breed =
    { name : String
    , pattern : Set ( Int, Int )
    , primary : Pigment
    , secondary : Pigment
    }


type alias Fish =
    { pattern : Set ( Int, Int )
    , rules : List ( Bool, Pattern )
    , primary : Pigment
    , secondary : Pigment
    , size : Int
    }


type BitColor
    = None
    | Black
    | Primary
    | Secondary


getBreedId : { breed | primary : Pigment, secondary : Pigment } -> BreedId
getBreedId breed =
    let
        fromBool : Bool -> Int
        fromBool b =
            if b then
                1

            else
                0
    in
    ( ( fromBool breed.primary.red
      , fromBool breed.primary.yellow
      , fromBool breed.primary.blue
      )
    , ( fromBool breed.secondary.red
      , fromBool breed.secondary.yellow
      , fromBool breed.secondary.blue
      )
    )


defaultBreed : Breed
defaultBreed =
    { name = "Common Goldfish"
    , pattern = Set.empty
    , primary = { red = False, yellow = False, blue = False }
    , secondary = { red = True, yellow = True, blue = False }
    }


new : Breed -> Fish
new args =
    let
        computedRules =
            args.pattern |> computeRules
    in
    { pattern = args.pattern
    , rules = computedRules
    , primary = args.primary
    , secondary = args.secondary
    , size = 1
    }


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


fromParents : Fish -> Fish -> Generator Fish
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

        randPigment pigment1 pigment2 =
            Random.map3
                (\b1 b2 b3 ->
                    let
                        chance =
                            0.497

                        fun p1 p2 b =
                            if b < chance then
                                p1

                            else if b < chance * 2 then
                                p2

                            else if b < chance * 2 + (1 - chance * 2) / 2 then
                                True

                            else
                                False
                    in
                    { blue = fun pigment1.blue pigment2.blue b1
                    , yellow = fun pigment1.yellow pigment2.yellow b2
                    , red = fun pigment1.red pigment2.red b3
                    }
                )
                (Random.float 0 1)
                (Random.float 0 1)
                (Random.float 0 1)

        randPattern =
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
    in
    Random.map3
        (\pattern primary secondary ->
            new
                { defaultBreed
                    | pattern = Set.fromList pattern
                    , primary = primary
                    , secondary = secondary
                }
        )
        randPattern
        (randPigment fish1.primary fish2.primary)
        (randPigment fish1.secondary fish2.secondary)
