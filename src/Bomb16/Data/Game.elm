module Bomb16.Data.Game exposing (Game, init, isOver, update)

import Bomb16.Data.Cell as Cell exposing (Cell(..))
import Bomb16.Data.Input as Input exposing (Direction(..))
import Bomb16.Data.World as World
import Dict exposing (Dict)
import Random exposing (Generator)


type alias Game =
    { score : Int
    , world : Dict ( Int, Int ) Cell
    , updateThisTurn : Bool
    }


init : Generator Game
init =
    Random.map
        (\world ->
            { score = 0
            , world = world
            , updateThisTurn = True
            }
        )
        World.generate


resolveConflict : ( Maybe Cell, Maybe Cell ) -> Maybe ( Maybe Cell, Maybe Cell, Int )
resolveConflict ( maybeCell1, maybeCell2 ) =
    let
        fun ( c1, c2 ) =
            case ( c1, c2 ) of
                ( Effect _, Effect _ ) ->
                    Just ( Nothing, Nothing, 0 )

                ( Effect _, _ ) ->
                    Just ( Nothing, Just c2, 0 )

                ( Sword s, Monster m ) ->
                    if m == s then
                        Just ( Nothing, Just <| Sword <| s + m, 0 )

                    else if m < s then
                        Nothing

                    else
                        Just ( Nothing, Just <| Monster m, 0 )

                ( Monster m1, Monster m2 ) ->
                    if m1 == m2 then
                        Just ( Nothing, Just <| Monster <| m1 + m2, m1 + m2 )

                    else
                        Nothing

                ( Wall w1, Wall w2 ) ->
                    Just ( Nothing, Just <| Wall <| w1 + w2, w1 + w2 )

                ( Wall w, Bomb b ) ->
                    if w > b then
                        Just ( Just <| Effect "💥", Just <| Wall <| w - b, 0 )

                    else if w < b then
                        Just ( Just <| Effect "💥", Just <| Bomb <| b - w, 0 )

                    else
                        Just ( Just <| Effect "💥", Just <| Effect "💥", 0 )

                ( _, Bomb b ) ->
                    if b > 1 then
                        Just ( Just <| Effect "💥", Just <| Bomb <| b - 1, 0 )

                    else
                        Just ( Just <| Effect "💥", Just <| Effect "💥", 0 )

                _ ->
                    Nothing
    in
    maybeCell1
        |> Maybe.andThen
            (\cell1 ->
                case maybeCell2 of
                    Nothing ->
                        case cell1 of
                            Effect _ ->
                                Just ( Nothing, Nothing, 0 )

                            _ ->
                                Just ( Nothing, Just cell1, 0 )

                    Just cell2 ->
                        if (cell1 |> Cell.order) <= (cell2 |> Cell.order) then
                            fun ( cell1, cell2 )

                        else
                            fun ( cell2, cell1 )
            )


updateCells : ( Int, Int ) -> ( Int, Int ) -> Game -> Game
updateCells pos1 pos2 game =
    case
        ( game.world |> Dict.get pos1
        , game.world |> Dict.get pos2
        )
            |> resolveConflict
    of
        Just ( cell1, cell2, addScore ) ->
            { game
                | world =
                    game.world
                        |> Dict.update pos1 (always cell1)
                        |> Dict.update pos2 (always cell2)
                , score = game.score + addScore
                , updateThisTurn = True
            }

        Nothing ->
            game


update : { direction : Direction, index : Int } -> Game -> Game
update arg game =
    World.listFromDirection arg.direction arg.index
        |> List.foldl
            (\( x, y ) ->
                updateCells
                    ( x, y )
                    (Input.fromDirection arg.direction
                        |> Tuple.mapBoth ((+) x) ((+) y)
                    )
            )
            game


isOver : Game -> Bool
isOver game =
    [ Up, Down, Left, Right ]
        |> List.any
            (\direction ->
                List.range 1 (World.size - 1)
                    |> List.foldl
                        (\index ->
                            update { direction = direction, index = index }
                        )
                        { game | updateThisTurn = False }
                    |> .updateThisTurn
            )
        |> not
