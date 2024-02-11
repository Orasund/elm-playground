module BugSpecies exposing (..)

import Random exposing (Generator)


type BugSpecies
    = Beetle
    | LadyBeetle
    | Cockroach
    | Snail
    | Grasshopper
    | Spider
    | Worm
    | Caterpillar
    | Ant
    | Butterfly


list : List BugSpecies
list =
    [ Worm
    , Caterpillar
    , Grasshopper
    , Snail
    , Beetle
    , Cockroach
    , LadyBeetle
    , Spider
    , Ant
    , Butterfly
    ]


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
            "🪲"

        LadyBeetle ->
            "🐞"

        Cockroach ->
            "🪳"

        Snail ->
            "🐌"

        Grasshopper ->
            "🦗"

        Spider ->
            "🕷️"

        Worm ->
            "🪱"

        Caterpillar ->
            "🐛"

        Ant ->
            "🐜"

        Butterfly ->
            "🦋"
