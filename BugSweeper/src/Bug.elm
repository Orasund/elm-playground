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
    | Mosquito
    | Bee


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
    , Mosquito
    , Bee
    ]
        |> List.sortBy
            (\bug ->
                -(requirementsOf bug
                    |> List.map Tuple.first
                    |> List.sum
                 )
            )


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

        Mosquito ->
            "🦟"

        Bee ->
            "🐝"


requirementsOf : Bug -> List ( Int, Maybe Object )
requirementsOf bug =
    case bug of
        Ant ->
            [ ( 3, Nothing ) ]

        Caterpillar ->
            [ ( 3, Just Leaf ) ]

        Worm ->
            [ ( 3, Just Stone ) ]

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
            [ ( 2, Just Wood ) ]

        Cockroach ->
            [ ( 1, Just Wood )
            , ( 1, Just Stone )
            ]

        Bee ->
            [ ( 1, Just Leaf )
            , ( 1, Nothing )
            ]

        Mosquito ->
            [ ( 1, Nothing ) ]

        Spider ->
            [ ( 1, Just Wood ) ]

        Butterfly ->
            []
