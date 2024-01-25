module Game.Update exposing (..)

import Cell exposing (Building(..))
import Dict
import Game exposing (Game)
import Random exposing (Generator)


shuffle : List a -> Generator (List a)
shuffle list =
    let
        anyInt : Generator Int
        anyInt =
            Random.int Random.minInt Random.maxInt
    in
    Random.map
        (\independentSeed ->
            list
                |> List.foldl
                    (\item ( acc, seed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step anyInt seed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], independentSeed )
                |> Tuple.first
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
        )
        Random.independentSeed


possibleMovesOf : Int -> Game -> List Game
possibleMovesOf id game =
    game.items
        |> Dict.get id
        |> Maybe.map
            (\item ->
                case game.cells |> Dict.get item.pos |> Maybe.map .building of
                    Just (Consumer _) ->
                        []

                    _ ->
                        [ Tuple.mapFirst ((+) -1) item.pos
                        , Tuple.mapFirst ((+) 1) item.pos
                        , Tuple.mapSecond ((+) -1) item.pos
                        , Tuple.mapSecond ((+) 1) item.pos
                        ]
                            |> List.filterMap
                                (\pos ->
                                    Game.moveTo id pos game
                                )
            )
        |> Maybe.withDefault []


updateItems : Game -> Generator Game
updateItems game =
    game.items
        |> Dict.keys
        |> shuffle
        |> Random.andThen
            (\list ->
                list
                    |> List.foldl
                        (\id ->
                            Random.andThen
                                (\g ->
                                    case possibleMovesOf id g of
                                        head :: tail ->
                                            Random.uniform head tail

                                        [] ->
                                            Random.constant g
                                )
                        )
                        (Random.constant game)
            )


tick : Game -> Generator Game
tick game =
    game.cells
        |> Dict.foldl
            (\_ cell ->
                case ( cell.building, cell.item ) of
                    ( Consumer color, Just id ) ->
                        Game.consume color id

                    _ ->
                        identity
            )
            game
        |> updateItems
        |> Random.map
            (\g ->
                g.cells
                    |> Dict.foldl
                        (\pos cell ->
                            case ( cell.building, cell.item ) of
                                ( Producer color, Nothing ) ->
                                    Game.produce color pos

                                _ ->
                                    identity
                        )
                        g
            )
