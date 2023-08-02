module Path exposing (PathBuilder, build, toDict)

import Cell exposing (Cell(..))
import Dict exposing (Dict)
import Dir
import Level exposing (Level)
import RelativePos exposing (RelativePos)
import Set exposing (Set)
import Stage exposing (Stage)


type alias PathBuilder =
    List
        { from : RelativePos
        , to : RelativePos
        , path : List RelativePos
        , id : Int
        }


stepThroughPath :
    Level
    -> Dict ( Int, Int ) Cell
    -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ), id : Int }
    -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ), id : Int }
stepThroughPath level grid =
    Maybe.andThen
        (\{ pos, to, path, id } ->
            grid
                |> Dict.get pos
                |> Maybe.andThen
                    (\cell ->
                        case cell of
                            ConnectionCell connection ->
                                connection.sendsTo
                                    |> Dict.toList
                                    |> List.map
                                        (\( k, v ) ->
                                            ( k
                                                |> RelativePos.toDir level
                                                |> Dir.addTo pos
                                            , v
                                            )
                                        )
                                    |> Dict.fromList
                                    |> Dict.get to
                                    |> Maybe.map
                                        (\{ from } ->
                                            { pos =
                                                from
                                                    |> RelativePos.toDir level
                                                    |> Dir.addTo pos
                                            , path = pos :: path
                                            , to = pos
                                            , id = id
                                            }
                                        )

                            Origin ->
                                Just
                                    { pos = pos
                                    , path = pos :: path
                                    , to = pos
                                    , id = id
                                    }

                            Target target ->
                                target.dir
                                    |> Maybe.map
                                        (\from ->
                                            { pos = from
                                            , path = pos :: path
                                            , to = pos
                                            , id = target.id
                                            }
                                        )

                            _ ->
                                Nothing
                    )
        )


toDict : PathBuilder -> Dict RelativePos (Set Int)
toDict list =
    let
        insert id pos =
            Dict.update pos
                (\maybe ->
                    (case maybe of
                        Nothing ->
                            Set.singleton id

                        Just l ->
                            Set.insert id l
                    )
                        |> Just
                )
    in
    list
        |> List.foldl
            (\{ path, id } d ->
                path |> List.foldl (insert id) d
            )
            Dict.empty


build : Level -> Stage -> PathBuilder
build level stage =
    stage.targets
        |> List.filterMap
            (\target ->
                List.range 0 16
                    |> List.foldl (\_ -> stepThroughPath level stage.grid)
                        (Just
                            { pos = target
                            , to = target
                            , path = []
                            , id = 0
                            }
                        )
                    |> Maybe.map
                        (\{ pos, path, id } ->
                            { from = RelativePos.fromTuple target
                            , to = RelativePos.fromTuple pos
                            , path = path |> List.map RelativePos.fromTuple
                            , id = id
                            }
                        )
            )
