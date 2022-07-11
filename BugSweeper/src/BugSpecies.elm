module BugSpecies exposing (..)

import Random exposing (Generator)


type BugSpecies
    = Beetle
    | LadyBeetle
    | Cockroach
    | Snail
    | Grasshopper
    | Spider


list : List BugSpecies
list =
    [ Snail, Beetle, Cockroach, LadyBeetle, Grasshopper, Spider ]


generate : Int -> Generator BugSpecies
generate level =
    case
        list
            |> List.drop (level // 2)
            |> List.take (level + 1)
            |> List.reverse
            |> List.indexedMap (\i species -> ( ((i + 1) ^ 2 + (i + 1)) // 2 |> toFloat, species ))
    of
        head :: tail ->
            Random.weighted head tail

        [] ->
            Random.constant Snail


toString : BugSpecies -> String
toString species =
    case species of
        Beetle ->
            "\u{1FAB2}"

        LadyBeetle ->
            "🐞"

        Cockroach ->
            "\u{1FAB3}"

        Snail ->
            "🐌"

        Grasshopper ->
            "🦗"

        Spider ->
            "🕷️"
