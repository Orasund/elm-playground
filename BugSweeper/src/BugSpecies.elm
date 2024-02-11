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


list : List BugSpecies
list =
    [ Worm, Caterpillar, Ant, Snail, Beetle, Cockroach, LadyBeetle, Grasshopper, Spider ]


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


movement : ( Int, Int ) -> BugSpecies -> List ( Int, Int )
movement ( x, y ) species =
    let
        default =
            [ ( x, y - 1 )
            , ( x - 1, y )
            , ( x, y + 1 )
            , ( x + 1, y )
            ]
    in
    case species of
        Grasshopper ->
            [ ( x + 1, y + 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y + 1 )
            , ( x - 1, y - 1 )
            , ( x, y - 2 )
            , ( x - 2, y )
            , ( x, y + 2 )
            , ( x + 2, y )
            ]

        LadyBeetle ->
            [ ( x, y - 1 )
            , ( x - 1, y )
            , ( x, y + 1 )
            , ( x + 1, y )
            , ( x, y - 2 )
            , ( x - 2, y )
            , ( x, y + 2 )
            , ( x + 2, y )
            ]

        Beetle ->
            default

        Cockroach ->
            default

        Spider ->
            default

        Snail ->
            default

        _ ->
            []
