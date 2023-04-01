module WaveFunCollapse exposing (..)

import Dict exposing (Dict)
import Html exposing (a)
import Random
import Set exposing (Set)


type alias Rule a =
    { neighbors : List ( ( Int, Int ), a )
    , center : a
    }


type alias Builder a =
    { output : Dict ( Int, Int ) a
    , remaining : Dict ( Int, Int ) (List (Rule a))
    , nextPossibleSteps : Set ( Int, Int )
    }


generator : List (Rule a) -> List ( Int, Int ) -> Random.Generator (Dict ( Int, Int ) a)
generator rules list =
    new rules list |> build


new : List (Rule a) -> List ( Int, Int ) -> Builder a
new rules list =
    list
        |> List.map (\pos -> ( pos, rules ))
        |> Dict.fromList
        |> (\remaining ->
                { output = Dict.empty
                , remaining = remaining
                , nextPossibleSteps = list |> Set.fromList
                }
           )


build : Builder a -> Random.Generator (Dict ( Int, Int ) a)
build builder =
    if Set.isEmpty builder.nextPossibleSteps then
        builder
            |> .output
            |> Random.constant

    else
        step builder |> Random.andThen build


step : Builder a -> Random.Generator (Builder a)
step builder =
    case builder.nextPossibleSteps |> Set.toList of
        head :: tail ->
            Random.uniform head tail
                |> Random.andThen (\pos -> stepAt pos builder)

        [] ->
            Random.constant builder


stepAt : ( Int, Int ) -> Builder a -> Random.Generator (Builder a)
stepAt pos builder =
    builder.remaining
        |> Dict.get pos
        |> Maybe.map
            (\remaining ->
                collapse pos remaining builder
            )
        |> Maybe.map
            (Random.map checkRemaining)
        |> Maybe.withDefault (Random.constant builder)


checkRemaining : Builder a -> Builder a
checkRemaining builder =
    builder.remaining
        |> Dict.foldl
            (\k list args ->
                list
                    |> List.filter (isValidRule k builder.output)
                    |> (\remaining ->
                            let
                                entropy =
                                    List.length remaining

                                smallestEntropy =
                                    args.smallestEntropy
                                        |> Maybe.map (min entropy)
                                        |> Maybe.withDefault entropy
                            in
                            { remaining =
                                args.remaining
                                    |> Dict.insert k remaining
                            , smallestEntropy =
                                smallestEntropy
                                    |> Just
                            , nextPossibleSteps =
                                if smallestEntropy == entropy then
                                    if
                                        args.smallestEntropy
                                            |> Maybe.map ((==) smallestEntropy)
                                            |> Maybe.withDefault True
                                    then
                                        k :: args.nextPossibleSteps

                                    else
                                        [ k ]

                                else
                                    args.nextPossibleSteps
                            }
                       )
            )
            { remaining = Dict.empty
            , smallestEntropy = Nothing
            , nextPossibleSteps = []
            }
        |> (\{ remaining, nextPossibleSteps } ->
                { builder
                    | remaining = remaining
                    , nextPossibleSteps =
                        nextPossibleSteps
                            |> Set.fromList
                }
           )


isValidRule : ( Int, Int ) -> Dict ( Int, Int ) a -> Rule a -> Bool
isValidRule ( x, y ) dict rule =
    rule.neighbors
        |> List.all
            (\( pos, a ) ->
                pos
                    |> Tuple.mapBoth ((+) x) ((+) y)
                    |> (\p -> dict |> Dict.get p)
                    |> Maybe.map ((==) a)
                    |> Maybe.withDefault True
            )


collapse : ( Int, Int ) -> List (Rule a) -> Builder a -> Random.Generator (Builder a)
collapse ( x, y ) possibleRules builder =
    case possibleRules of
        head :: tail ->
            Random.uniform head tail
                |> Random.map
                    (\{ neighbors, center } ->
                        neighbors
                            |> List.foldl (\( p, a ) -> Dict.insert (p |> Tuple.mapBoth ((+) x) ((+) y)) a)
                                builder.output
                            |> Dict.insert ( x, y ) center
                            |> (\output ->
                                    { builder
                                        | output = output
                                        , remaining =
                                            neighbors
                                                |> List.foldl (\( p, _ ) -> Dict.remove (p |> Tuple.mapBoth ((+) x) ((+) y)))
                                                    builder.remaining
                                                |> Dict.remove ( x, y )
                                    }
                               )
                    )

        [] ->
            Random.constant { builder | remaining = builder.remaining |> Dict.remove ( x, y ) }
