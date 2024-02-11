module Game.Update exposing (moveBugs)

import BugSpecies exposing (BugSpecies(..))
import Dict
import Game exposing (Bug, Game)
import Random exposing (Generator)
import Random.List
import Tile exposing (Tile(..))


type alias Random a =
    Generator a


moveBugs : Game -> Random Game
moveBugs game =
    game.bugs
        |> Dict.toList
        |> List.filter (\( _, { visible } ) -> not visible)
        |> Random.List.shuffle
        |> Random.andThen
            (List.foldl
                (\( p, bug ) -> Random.andThen (moveBug p bug))
                (Random.constant game)
            )


moveBug : ( Int, Int ) -> Bug -> Game -> Random Game
moveBug ( x, y ) bug g =
    bug.species
        |> BugSpecies.movement ( x, y )
        |> List.filter Game.isValidPos
        |> List.filterMap
            (\p ->
                case ( g.grid |> Dict.get p, g.bugs |> Dict.get p ) of
                    ( Nothing, Nothing ) ->
                        Just ( p, Nothing )

                    ( Just Stone, Nothing ) ->
                        Just ( p, Just Stone )

                    ( Just SpiderWeb, Nothing ) ->
                        Just ( p, Just SpiderWeb )

                    ( _, _ ) ->
                        Nothing
            )
        |> (case bug.species of
                Cockroach ->
                    \list ->
                        let
                            ( stone, empty ) =
                                List.partition
                                    (\( _, maybe ) ->
                                        maybe == Just Stone
                                    )
                                    list
                        in
                        if List.isEmpty stone then
                            Random.constant empty

                        else
                            case empty of
                                h :: t ->
                                    Random.uniform h t
                                        |> Random.map (\e -> e :: stone)

                                [] ->
                                    Random.constant stone

                Grasshopper ->
                    \list ->
                        let
                            ( stone, empty ) =
                                List.partition
                                    (\( _, maybe ) ->
                                        maybe == Just Stone
                                    )
                                    list
                        in
                        (if List.isEmpty empty then
                            stone

                         else
                            empty
                        )
                            |> Random.constant

                Snail ->
                    \list ->
                        let
                            ( _, empty ) =
                                List.partition
                                    (\( _, maybe ) ->
                                        maybe == Just Stone
                                    )
                                    list
                        in
                        (case g.grid |> Dict.get ( x, y ) of
                            Just Stone ->
                                empty

                            _ ->
                                []
                        )
                            |> Random.constant

                _ ->
                    Random.constant
           )
        |> Random.andThen Random.List.choose
        |> Random.andThen
            (\( maybe, _ ) ->
                case bug.species of
                    Spider ->
                        Random.int 0 4
                            |> Random.map
                                (\int ->
                                    if int == 0 then
                                        { g
                                            | grid =
                                                g.grid
                                                    |> Dict.update ( x, y ) (\m -> m |> Maybe.withDefault SpiderWeb |> Just)
                                            , bugs =
                                                maybe
                                                    |> Maybe.map
                                                        (\( p, _ ) ->
                                                            g.bugs
                                                                |> Dict.remove ( x, y )
                                                                |> Dict.insert p { bug | visible = False }
                                                        )
                                                    |> Maybe.withDefault g.bugs
                                        }

                                    else
                                        { g
                                            | bugs =
                                                maybe
                                                    |> Maybe.map
                                                        (\( p, _ ) ->
                                                            g.bugs
                                                                |> Dict.remove ( x, y )
                                                                |> Dict.insert p { bug | visible = False }
                                                        )
                                                    |> Maybe.withDefault g.bugs
                                        }
                                )

                    _ ->
                        maybe
                            |> Maybe.map
                                (\( p, _ ) ->
                                    g.bugs
                                        |> Dict.remove ( x, y )
                                        |> Dict.insert p { bug | visible = False }
                                )
                            |> Maybe.withDefault g.bugs
                            |> Random.constant
                            |> Random.map (\bugs -> { g | bugs = bugs })
            )
