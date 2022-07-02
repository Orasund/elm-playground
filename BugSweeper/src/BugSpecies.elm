module BugSpecies exposing (..)

import Random exposing (Generator)


type BugSpecies
    = Beetle
    | LadyBeetle
    | Cockroach
    | Snail
    | Grasshopper


generate : Generator BugSpecies
generate =
    [ ( 8, Beetle ), ( 4, Cockroach ), ( 2, LadyBeetle ), ( 1, Grasshopper ) ]
        |> Random.weighted ( 16, Snail )


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
