module BugSpecies exposing (..)

import Random exposing (Generator)
import Tile exposing (Tile(..))


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


requirementsOf : BugSpecies -> List ( Int, Maybe Tile )
requirementsOf bug =
    case bug of
        Ant ->
            [ ( 4, Nothing ) ]

        Caterpillar ->
            [ ( 3, Just Leaf ) ]

        Worm ->
            [ ( 4, Just Stone ) ]

        Snail ->
            [ ( 1, Just Stone ) ]

        Grasshopper ->
            [ ( 1, Just Leaf ) ]

        Beetle ->
            [ ( 1, Just Leaf )
            , ( 1, Just Wood )
            ]

        LadyBeetle ->
            [ ( 1, Just Wood ) ]

        Spider ->
            [ ( 1, Just SpiderWeb ) ]

        Cockroach ->
            [ ( 1, Just Wood )
            , ( 1, Just Stone )
            ]

        Butterfly ->
            []
