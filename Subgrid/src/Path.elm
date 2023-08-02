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
    -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ) }
    -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ) }
stepThroughPath level grid =
    Maybe.andThen
        (\{ pos, to, path } ->
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
                                            }
                                        )

                            Origin ->
                                Just
                                    { pos = pos
                                    , path = pos :: path
                                    , to = pos
                                    }

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
            (\pos ->
                case stage.grid |> Dict.get pos of
                    Just (Target target) ->
                        Just
                            { pos = pos
                            , id = target.id
                            , from =
                                target.dir
                                    |> List.map
                                        (\relativePos ->
                                            relativePos
                                                |> RelativePos.toDir level
                                                |> Dir.addTo pos
                                        )
                            }

                    _ ->
                        Nothing
            )
        |> List.concatMap
            (\target ->
                target.from
                    |> List.filterMap
                        (\from ->
                            List.range
                                0
                                16
                                |> List.foldl (\_ -> stepThroughPath level stage.grid)
                                    (Just
                                        { pos = from
                                        , to = target.pos
                                        , path = [ target.pos ]
                                        }
                                    )
                                |> Maybe.map
                                    (\{ pos, path } ->
                                        { from = RelativePos.fromTuple target.pos
                                        , to = RelativePos.fromTuple pos
                                        , path = path |> List.map RelativePos.fromTuple
                                        , id = target.id
                                        }
                                    )
                        )
                    |> (\list ->
                            if list |> List.map .to |> Set.fromList |> Set.size |> (==) 1 then
                                list

                            else
                                []
                       )
            )
