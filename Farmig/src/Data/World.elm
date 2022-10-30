module  Data.World exposing (generate)

import Dict exposing (Dict)
import  Data.Cell exposing (Cell(..))
import  Data.Food exposing (Food(..))
import  Data.Item exposing (Item(..))
import Random exposing (Generator)


{-| worldSize also specifies the difficulty (and the level)
-}
generate :
    { level : Int
    , worldSize : Int
    , startAtY : Int
    }
    -> Generator (Dict ( Int, Int ) Cell)
generate { level, worldSize, startAtY } =
    Dict.empty
        |> distribute
            { amount = ((worldSize - 2) * (worldSize - 2)) * 2 // 3
            , worldSize = worldSize
            , cells =
                if level <= 1 then
                    [ ( 50, Ground )
                    , ( 50, Wood )
                    ]

                else if level <= 4 then
                    [ ( 50, Ground )
                    , ( 30, Wood )
                    , ( 10, Seed Berry )
                    , ( 10, Item Water )
                    ]

                else if level <= 8 then
                    [ ( 50, Ground )
                    , ( 10, Wood )
                    , ( 10, Seed Berry )
                    , ( 15, Seed Carrot )
                    , ( 15, Item Water )
                    ]

                else if level <= 12 then
                    [ ( 30, Ground )
                    , ( 10, Wood )
                    , ( 25, Seed Carrot )
                    , ( 15, Seed Melon )
                    , ( 20, Item Water )
                    ]

                else if level <= 16 then
                    [ ( 10, Ground )
                    , ( 10, Wood )
                    , ( 25, Seed Carrot )
                    , ( 20, Seed Melon )
                    , ( 10, Seed Apple )
                    , ( 25, Item Water )
                    ]

                else if level <= 20 then
                    [ ( 30, Ground )
                    , ( 10, Wood )
                    , ( 15, Seed Carrot )
                    , ( 15, Seed Melon )
                    , ( 10, Seed Apple )
                    , ( 20, Item Water )
                    ]

                else if level <= 24 then
                    [ ( 50, Ground )
                    , ( 10, Wood )
                    , ( 5, Seed Carrot )
                    , ( 10, Seed Melon )
                    , ( 10, Seed Apple )
                    , ( 15, Item Water )
                    ]

                else
                    [ ( 70, Ground )
                    , ( 10, Wood )
                    , ( 5, Seed Carrot )
                    , ( 10, Seed Melon )
                    , ( 5, Item Water )
                    ]
            }
        |> (\world ->
                List.range 1 (level // 2)
                    |> List.foldl
                        (\_ ->
                            Random.andThen
                                (randomWalk
                                    { fillWith = Ground
                                    , steps = worldSize // 2
                                    , startAt = { x = 1, y = startAtY }
                                    , addEachStep = ( 1, 0 )
                                    , directions =
                                        [ ( 1, ( 0, -1 ) )
                                        , ( 1, ( 0, 1 ) )
                                        , ( 0.5, ( 1, -1 ) )
                                        , ( 0.5, ( 1, 1 ) )
                                        ]
                                    }
                                    >> Random.map Tuple.first
                                )
                        )
                        world
           )
        |> (\world ->
                Random.int (worldSize // 4) (worldSize * 3 // 4)
                    |> Random.andThen
                        (\riverAtX ->
                            List.range 1 (worldSize // 4)
                                |> List.foldl
                                    (\steps ->
                                        Random.andThen
                                            (randomWalk
                                                { fillWith = Item Water
                                                , steps = steps
                                                , startAt = { x = riverAtX, y = 1 }
                                                , addEachStep = ( 0, 1 )
                                                , directions =
                                                    [ ( 1, ( 0, -1 ) )
                                                    , ( 1, ( 0, 1 ) )
                                                    , ( 1, ( 1, 0 ) )
                                                    , ( 1, ( -1, 0 ) )
                                                    ]
                                                }
                                                >> Random.map Tuple.first
                                            )
                                    )
                                    world
                        )
           )
        |> Random.map
            (\world ->
                [ ( ( 0, startAtY ), Ground )
                , ( ( 1, startAtY ), Ground )
                ]
                    |> List.foldl
                        (\( pos, cell ) ->
                            Dict.insert pos cell
                        )
                        world
            )
        |> Random.andThen
            (randomWalk
                { fillWith = Ground
                , steps = worldSize - 2
                , startAt = { x = 1, y = startAtY }
                , addEachStep = ( 1, 0 )
                , directions =
                    [ ( 1, ( 0, -1 ) )
                    , ( 1, ( 0, 1 ) )
                    ]
                }
                >> Random.map
                    (\( world, endPos ) ->
                        world
                            |> Dict.insert endPos Goal
                    )
            )
        |> Random.andThen
            (distribute
                { amount = level * 2
                , worldSize = worldSize
                , cells =
                    if level <= 1 then
                        [ ( 100, Food Cherry ) ]

                    else if level <= 4 then
                        [ ( 70, Food Cherry )
                        , ( 30, Food Berry )
                        ]

                    else if level <= 8 then
                        [ ( 50, Food Cherry )
                        , ( 20, Food Berry )
                        , ( 40, Item Axe )
                        ]

                    else if level <= 12 then
                        [ ( 30, Food Cherry )
                        , ( 10, Food Berry )
                        , ( 50, Item Axe )
                        , ( 10, Rabbit )
                        ]

                    else if level <= 16 then
                        [ ( 10, Food Cherry )
                        , ( 60, Item Axe )
                        , ( 30, Rabbit )
                        ]

                    else if level <= 20 then
                        [ ( 70, Item Axe )
                        , ( 20, Rabbit )
                        , ( 10, Ground )
                        ]

                    else if level <= 24 then
                        [ ( 60, Item Axe )
                        , ( 10, Rabbit )
                        , ( 30, Ground )
                        ]

                    else
                        [ ( 50, Item Axe )
                        , ( 50, Ground )
                        ]
                }
            )


distribute :
    { amount : Int
    , worldSize : Int
    , cells : List ( Float, Cell )
    }
    -> Dict ( Int, Int ) Cell
    -> Generator (Dict ( Int, Int ) Cell)
distribute { amount, worldSize, cells } dict =
    let
        randomCell : ( Float, Cell ) -> List ( Float, Cell ) -> Generator Cell
        randomCell head tail =
            Random.weighted head tail

        randomPosition : Generator ( Int, Int )
        randomPosition =
            Random.map2 Tuple.pair
                (Random.int 1 (worldSize - 2))
                (Random.int 1 (worldSize - 2))
    in
    case cells of
        head :: tail ->
            Random.map2 Tuple.pair
                randomPosition
                (randomCell head tail)
                |> Random.list amount
                |> Random.map
                    (\list ->
                        Dict.union (list |> Dict.fromList) dict
                    )

        [] ->
            Random.constant dict


step :
    Cell
    -> ( Int, Int )
    -> ( Dict ( Int, Int ) Cell, ( Int, Int ) )
    -> ( Dict ( Int, Int ) Cell, ( Int, Int ) )
step fillWith ( right, down ) ( dict, ( x, y ) ) =
    let
        newPos =
            ( x + right, y + down )
    in
    ( dict
        |> Dict.insert newPos fillWith
    , newPos
    )


randomWalk :
    { fillWith : Cell
    , steps : Int
    , startAt : { x : Int, y : Int }
    , addEachStep : ( Int, Int )
    , directions : List ( Float, ( Int, Int ) )
    }
    -> Dict ( Int, Int ) Cell
    -> Generator ( Dict ( Int, Int ) Cell, ( Int, Int ) )
randomWalk { fillWith, steps, startAt, addEachStep, directions } d =
    let
        randomDirection : Generator ( Int, Int )
        randomDirection =
            case directions of
                head :: tail ->
                    Random.weighted head tail

                [] ->
                    Random.constant ( 0, 0 )
    in
    randomDirection
        |> Random.list steps
        |> Random.map
            (List.foldl
                (\pos -> step fillWith pos >> step fillWith addEachStep)
                ( d, ( startAt.x, startAt.y ) )
            )
