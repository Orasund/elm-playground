module WaveFunCollapse exposing (..)

import Dict exposing (Dict)
import Html exposing (a)
import Random
import Set exposing (Set)


type alias Rule a =
    { neighbors : List ( ( Int, Int ), a )
    , center : a
    }


type alias Snapshot comparable =
    { output : Dict ( Int, Int ) comparable
    , remaining : Dict ( Int, Int ) (Set comparable)
    }


type alias Builder comparable =
    { output : Dict ( Int, Int ) comparable
    , remaining : Dict ( Int, Int ) (Set comparable)
    , rules : Dict comparable (List (Rule comparable))
    , nextPossibleSteps : Set ( Int, Int )
    , path : List ( ( Int, Int ), comparable, Snapshot comparable )
    }


generator : List (Rule comparable) -> List ( Int, Int ) -> Random.Generator (Maybe (Dict ( Int, Int ) comparable))
generator rules list =
    new rules list |> build


new : List (Rule comparable) -> List ( Int, Int ) -> Builder comparable
new ruleList list =
    let
        rules =
            ruleList
                |> List.foldl
                    (\rule ->
                        Dict.update rule.center
                            (\maybe ->
                                maybe
                                    |> Maybe.withDefault []
                                    |> (::) rule
                                    |> Just
                            )
                    )
                    Dict.empty

        values =
            Dict.keys rules |> Set.fromList
    in
    list
        |> List.map (\pos -> ( pos, values ))
        |> Dict.fromList
        |> (\remaining ->
                { output = Dict.empty
                , remaining = remaining
                , rules = rules
                , nextPossibleSteps = list |> Set.fromList
                , path = []
                }
           )


build : Builder comparable -> Random.Generator (Maybe (Dict ( Int, Int ) comparable))
build builder =
    if Set.isEmpty builder.nextPossibleSteps then
        builder.output
            |> Just
            |> Random.constant

    else
        step builder
            |> Random.andThen
                (\maybe ->
                    case maybe of
                        Just a ->
                            build a

                        Nothing ->
                            Random.constant Nothing
                )


step : Builder comparable -> Random.Generator (Maybe (Builder comparable))
step builder =
    case builder.nextPossibleSteps |> Set.toList of
        head :: tail ->
            Random.uniform head tail
                |> Random.andThen
                    (\pos ->
                        stepAt pos builder
                            |> Maybe.map (Random.map Just)
                            |> Maybe.withDefault (Random.constant Nothing)
                    )

        [] ->
            Random.constant Nothing


stepAt : ( Int, Int ) -> Builder comparable -> Maybe (Random.Generator (Builder comparable))
stepAt pos builder =
    let
        values =
            builder.remaining
                |> Dict.get pos
                |> Maybe.withDefault Set.empty
                |> Set.toList

        snapshot =
            { output = builder.output
            , remaining = builder.remaining
            }
    in
    case values of
        head :: tail ->
            Random.uniform head tail
                |> Random.map
                    (\value ->
                        collapse pos
                            value
                            { builder | path = ( pos, value, snapshot ) :: builder.path }
                    )
                |> Random.map checkRemaining
                |> Just

        [] ->
            builder |> backtrack |> Maybe.map Random.constant


checkRemaining : Builder comparable -> Builder comparable
checkRemaining builder =
    builder.remaining
        |> Dict.foldl
            (\pos values args ->
                values
                    |> Set.filter
                        (\rule ->
                            builder.rules
                                |> Dict.get rule
                                |> Maybe.withDefault []
                                |> List.any (isValidRule pos builder.output)
                        )
                    |> (\remaining ->
                            let
                                entropy =
                                    Set.size remaining

                                smallestEntropy =
                                    args.smallestEntropy
                                        |> Maybe.map (min entropy)
                                        |> Maybe.withDefault entropy
                            in
                            { remaining =
                                args.remaining
                                    |> Dict.insert pos remaining
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
                                        pos :: args.nextPossibleSteps

                                    else
                                        [ pos ]

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


backtrack : Builder comparable -> Maybe (Builder comparable)
backtrack builder =
    case builder.path of
        ( pos, value, snapshot ) :: tail ->
            { builder
                | output = snapshot.output
                , remaining =
                    snapshot.remaining
                        |> Dict.update pos
                            (\maybe -> maybe |> Maybe.map (Set.remove value))
                , path = tail
            }
                |> checkRemaining
                |> Just

        [] ->
            Nothing


collapse : ( Int, Int ) -> a -> Builder a -> Builder a
collapse pos value builder =
    { builder
        | output = Dict.insert pos value builder.output
        , remaining =
            Dict.remove pos builder.remaining
    }
