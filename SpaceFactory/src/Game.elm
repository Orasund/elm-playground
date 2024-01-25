module Game exposing (..)

import Cell exposing (Building(..), Cell)
import Color exposing (Color(..))
import Dict exposing (Dict)


type alias Game =
    { cells : Dict ( Int, Int ) Cell
    , width : Int
    , height : Int
    , items : Dict Int { pos : ( Int, Int ), color : Color }
    , nextId : Int
    }


new : Game
new =
    { cells =
        [ ( ( 0, 0 ), Cell.fromBuilding (Producer Red) )
        , ( ( 0, 2 ), Cell.fromBuilding (Producer Blue) )
        , ( ( 1, 0 ), Cell.fromBuilding (Consumer Blue) )
        , ( ( 1, 2 ), Cell.fromBuilding (Consumer Red) )
        , ( ( 0, 1 ), Cell.fromBuilding Pipe )
        , ( ( 1, 1 ), Cell.fromBuilding Pipe )
        ]
            |> Dict.fromList
    , width = 3
    , height = 3
    , items = Dict.empty
    , nextId = 0
    }


produce : Color -> ( Int, Int ) -> Game -> Game
produce color pos game =
    game.cells
        |> Dict.get pos
        |> Maybe.andThen
            (\cell ->
                if cell.item == Nothing then
                    { game
                        | cells =
                            game.cells
                                |> Dict.insert pos
                                    { cell | item = Just game.nextId }
                        , items =
                            game.items
                                |> Dict.insert game.nextId
                                    { pos = pos, color = color }
                        , nextId = game.nextId + 1
                    }
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault game


consume : Color -> Int -> Game -> Game
consume color id game =
    game.items
        |> Dict.get id
        |> Maybe.andThen
            (\item ->
                if item.color == color then
                    game.cells
                        |> Dict.get item.pos
                        |> Maybe.map
                            (\cell ->
                                { game
                                    | cells =
                                        game.cells
                                            |> Dict.insert item.pos
                                                { cell | item = Nothing }
                                    , items =
                                        game.items
                                            |> Dict.remove id
                                }
                            )

                else
                    Nothing
            )
        |> Maybe.withDefault game


moveTo : Int -> ( Int, Int ) -> Game -> Maybe Game
moveTo id to game =
    game.items
        |> Dict.get id
        |> Maybe.andThen
            (\item ->
                Maybe.map2
                    (\fromCell toCell ->
                        if
                            (case toCell.building of
                                Producer _ ->
                                    False

                                Consumer color ->
                                    item.color == color

                                _ ->
                                    True
                            )
                                && (toCell.item == Nothing)
                                && (fromCell.item == Just id)
                        then
                            { game
                                | cells =
                                    game.cells
                                        |> Dict.insert item.pos { fromCell | item = Nothing }
                                        |> Dict.insert to { toCell | item = Just id }
                                , items = game.items |> Dict.insert id { item | pos = to }
                            }
                                |> Just

                        else
                            Nothing
                    )
                    (Dict.get item.pos game.cells)
                    (Dict.get to game.cells)
                    |> Maybe.andThen identity
            )
