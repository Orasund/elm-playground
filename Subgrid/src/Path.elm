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
        , targetId : Int
        , originId : Int
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

                            Origin _ ->
                                Just
                                    { pos = pos
                                    , path = pos :: path
                                    , to = pos
                                    }

                            _ ->
                                Nothing
                    )
        )


toDict : PathBuilder -> Dict RelativePos { targetIds : Set Int }
toDict list =
    let
        insert id pos =
            Dict.update pos
                (\maybe ->
                    (case maybe of
                        Nothing ->
                            { targetIds = Set.singleton id }

                        Just { targetIds } ->
                            { targetIds = Set.insert id targetIds }
                    )
                        |> Just
                )
    in
    list
        |> List.foldl
            (\{ path, targetId } d ->
                path |> List.foldl (insert targetId) d
            )
            Dict.empty


fromTarget : Level -> Stage -> ( Int, Int ) -> Maybe { pos : ( Int, Int ), targetId : Int, from : List ( Int, Int ), originId : Int }
fromTarget level stage pos =
    case stage.grid |> Dict.get pos of
        Just (Target target) ->
            let
                from =
                    target.sendsTo
                        |> Dict.keys
                        |> List.map
                            (\relativePos ->
                                relativePos
                                    |> RelativePos.toDir level
                                    |> Dir.addTo pos
                            )

                maybeOriginId =
                    case
                        target.sendsTo
                            |> Dict.values
                            |> List.map .originId
                            |> Set.fromList
                            |> Set.toList
                    of
                        [ a ] ->
                            Just a

                        _ ->
                            Nothing
            in
            maybeOriginId
                |> Maybe.map
                    (\originId ->
                        { pos = pos
                        , targetId = target.id
                        , from = from
                        , originId = originId
                        }
                    )

        _ ->
            Nothing


build : Level -> Stage -> PathBuilder
build level stage =
    stage.targets
        |> Dict.keys
        |> List.filterMap (fromTarget level stage)
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
                                        , targetId = target.targetId
                                        , originId = target.originId
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
