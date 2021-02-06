module Farmig.Data.World exposing (generate)

import Dict exposing (Dict)
import Farmig.Data.Cell exposing (Cell(..))
import Farmig.Data.Food exposing (Food(..))
import Farmig.Data.Item exposing (Item(..))
import Random exposing (Generator)


{-| worldSize also specifies the difficulty (and the level)
-}
generate :
    { worldSize : Int
    , startAtY : Int
    }
    -> Generator (Dict ( Int, Int ) Cell)
generate { worldSize, startAtY } =
    [ ( ( 0, startAtY ), Ground )
    , ( ( 1, startAtY ), Ground )
    ]
        |> Dict.fromList
        |> distribute
            { amount = ((worldSize - 2) * (worldSize - 2)) * 2 // 3
            , worldSize = worldSize
            , cells =
                [ ( 100, Ground )
                , ( 10 + 1 * toFloat worldSize, Item Water )
                , ( toFloat worldSize / 2, Seed Carrot )
                , ( 10, Seed Berry )
                , ( 10 + 1 * toFloat worldSize, Wood )
                ]
            }
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
            )
        |> Random.map
            (\( world, endPos ) ->
                world
                    |> Dict.insert endPos Goal
            )
        |> (\world ->
                Random.int 1 (worldSize - 2)
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
        |> Random.andThen
            (distribute
                { amount = worldSize - 4 // 2
                , worldSize = worldSize
                , cells =
                    [ ( 1, Food Cherry )
                    , ( toFloat <| (worldSize - 5) // 8, Item Axe )
                    , ( toFloat <| (worldSize - 5) // 8, Rabbit )
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
                        dict
                            |> Dict.union (list |> Dict.fromList)
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
