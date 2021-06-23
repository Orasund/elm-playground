module Bomb16.Data.World exposing (add, find, generate, isFull, listFromDirection, size)

import Dict exposing (Dict)
import Bomb16.Data.Cell exposing (Cell(..))
import Bomb16.Data.Input exposing (Direction(..))
import Random exposing (Generator)


size : Int
size =
    5


generate : Generator (Dict ( Int, Int ) Cell)
generate =
    let
        tupleGenerator =
            Random.map2 Tuple.pair
                (Random.int 0 (size - 1))
                (Random.int 0 (size - 1))
    in
    Random.map2
        (\player monster ->
            Dict.fromList
                [ ( player, Monster 1 ) --( player, Sword 1 )
                , ( monster, Monster 1 )
                ]
        )
        tupleGenerator
        tupleGenerator


add : { cell : Generator Cell, direction : Direction } -> Dict ( Int, Int ) Cell -> Generator (Dict ( Int, Int ) Cell)
add arg world =
    case
        listFromDirection arg.direction (size - 1)
            |> List.filter
                (\pos ->
                    world |> Dict.get pos |> (==) Nothing
                )
    of
        head :: tail ->
            Random.map2
                (\cell pos ->
                    world
                        |> Dict.update pos
                            (\maybeCell ->
                                case maybeCell of
                                    Nothing ->
                                        Just cell

                                    _ ->
                                        maybeCell
                            )
                )
                arg.cell
                (Random.uniform head tail)

        [] ->
            Random.constant world


row : Int -> List ( Int, Int )
row i =
    List.range 0 (size - 1)
        |> List.map (\x -> ( x, i ))


column : Int -> List ( Int, Int )
column i =
    List.range 0 (size - 1)
        |> List.map (\y -> ( i, y ))


listFromDirection : Direction -> Int -> List ( Int, Int )
listFromDirection direction index =
    case direction of
        Up ->
            row index

        Down ->
            row (size - 1 - index)

        Left ->
            column index

        Right ->
            column (size - 1 - index)


find : (Cell -> Bool) -> Dict ( Int, Int ) Cell -> Maybe ( ( Int, Int ), Cell )
find fun =
    Dict.filter (always fun)
        >> Dict.toList
        >> List.head


isFull : Dict ( Int, Int ) Cell -> Bool
isFull dict =
    List.range 0 (size - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (size - 1)
                    |> List.map
                        (\y ->
                            ( x, y )
                        )
            )
        |> List.any (\pos -> dict |> Dict.get pos |> (==) Nothing)
        |> not
