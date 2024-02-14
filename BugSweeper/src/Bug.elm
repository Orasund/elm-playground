module Bug exposing (..)

import Object exposing (Object(..))
import Random exposing (Generator)


type Bug
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


list : List Bug
list =
    [ Worm
    , Caterpillar
    , Snail
    , Grasshopper
    , Beetle
    , Cockroach
    , LadyBeetle
    , Spider
    , Ant
    , Butterfly
    ]


generate : Int -> Generator Bug
generate level =
    case
        list
            --|> List.drop (level // 2)
            |> List.take (level + 1)
            |> List.reverse
            |> List.indexedMap (\i species -> ( ((i + 1) ^ 2 + (i + 1)) // 2 |> toFloat, species ))
    of
        head :: tail ->
            Random.weighted head tail

        [] ->
            Random.constant Snail


toString : Bug -> String
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


requirementsOf : Bug -> List ( Int, Maybe Object )
requirementsOf bug =
    case bug of
        Ant ->
            [ ( 4, Nothing ) ]

        Caterpillar ->
            [ ( 3, Just Leaf ) ]

        Worm ->
            [ ( 4, Just Stone ) ]

        Snail ->
            [ ( 2, Just Stone )
            , ( 1, Just Leaf )
            ]

        Grasshopper ->
            [ ( 2, Just Leaf ) ]

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
