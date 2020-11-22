module Ecocards.Data.Bag exposing (findMinMaxSubset)

import Bag exposing (Bag)


findMinSubsetRec : ( Int, Int ) -> Bag Int -> Bag Int -> Maybe (Bag Int)
findMinSubsetRec ( minAmount, maxAmount ) remBag out =
    remBag
        |> Bag.foldl
            (\value _ maybeOut ->
                let
                    newMin =
                        minAmount
                            - value

                    newMax =
                        maxAmount - value
                in
                case maybeOut of
                    Just a ->
                        Just a

                    Nothing ->
                        if newMin <= 0 && newMax >= 0 then
                            --Solution Found
                            out |> Bag.insert 1 value |> Just

                        else if newMax < 0 then
                            --No Solution -> Skip
                            Nothing

                        else
                            --take another
                            findMinSubsetRec ( newMin, newMax )
                                (remBag |> Bag.remove 1 value)
                                (out |> Bag.insert 1 value)
            )
            Nothing


findMinSubset : ( Int, Int ) -> Bag Int -> Maybe (Bag Int)
findMinSubset intervall bag =
    Bag.empty |> findMinSubsetRec intervall bag


findMinMaxSubsetRec : Bag Int -> ( Int, Int ) -> Bag Int -> Bag Int -> Maybe { minBag : Bag Int, maxBag : Bag Int }
findMinMaxSubsetRec minBag ( minAmount, maxAmount ) remBag out =
    if remBag |> Bag.isEmpty then
        --continue with minBag
        findMinSubset ( minAmount, maxAmount )
            minBag
            |> Maybe.map
                (\minSolution ->
                    { maxBag = out
                    , minBag = minSolution
                    }
                )

    else
        remBag
            |> Bag.foldr
                (\value _ maybeOut ->
                    let
                        newMin =
                            minAmount
                                - value

                        newMax =
                            maxAmount - value
                    in
                    case maybeOut of
                        Just a ->
                            Just a

                        Nothing ->
                            if newMin <= 0 && newMax >= 0 then
                                --Solution Found
                                let
                                    newOut =
                                        out |> Bag.insert 1 value

                                    currentSolution =
                                        Just
                                            { maxBag = newOut
                                            , minBag = Bag.empty
                                            }
                                in
                                if remBag |> Bag.remove 1 value |> Bag.isEmpty then
                                    --better bail out now
                                    currentSolution

                                else
                                    --Find a better solution
                                    case
                                        findMinMaxSubsetRec minBag
                                            ( newMin, newMax )
                                            (remBag |> Bag.remove 1 value)
                                            newOut
                                    of
                                        Just solution ->
                                            --We might have overdone it
                                            if newOut == solution.minBag then
                                                --We have
                                                currentSolution

                                            else
                                                Just solution

                                        Nothing ->
                                            --Else return the found solution
                                            currentSolution

                            else if newMax < 0 then
                                --No Solution
                                -- -> continue in minBag
                                findMinSubset ( minAmount, maxAmount ) minBag
                                    |> Maybe.map
                                        (\minSolution ->
                                            { maxBag = out
                                            , minBag = minSolution
                                            }
                                        )

                            else
                                --take another
                                findMinMaxSubsetRec minBag
                                    ( newMin, newMax )
                                    (remBag |> Bag.remove 1 value)
                                    (out |> Bag.insert 1 value)
                )
                Nothing


{-| given an intervall, and two bags (minBag,maxBag),

returns a subset of the bags such that the sum lyes within the intervall.

While choosing the Elements, the partial sum of the maxBag is tried to be maximized,
while the partial sum of the minBag is being minimized.

-}
findMinMaxSubset : ( Int, Int ) -> { minBag : Bag Int, maxBag : Bag Int } -> Maybe { minBag : Bag Int, maxBag : Bag Int }
findMinMaxSubset intervall { minBag, maxBag } =
    Bag.empty |> findMinMaxSubsetRec minBag intervall maxBag
