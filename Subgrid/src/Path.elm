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
    { level : Level
    , grid : Dict ( Int, Int ) Cell
    , originId : Int
    }
    -> Maybe { pos : ( Int, Int ), path : List ( Int, Int ) }
    -> Maybe { pos : ( Int, Int ), path : List ( Int, Int ) }
stepThroughPath args =
    Maybe.andThen
        (\{ pos, path } ->
            args.grid
                |> Dict.get pos
                |> Maybe.andThen
                    (\cell ->
                        case cell of
                            ConnectionCell connection ->
                                connection.sendsTo
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( _, v ) ->
                                            if v.originId == args.originId then
                                                v.from
                                                    |> RelativePos.toDir args.level
                                                    |> Dir.addTo pos
                                                    |> Just

                                            else
                                                Nothing
                                        )
                                    |> List.head
                                    |> Maybe.map
                                        (\from ->
                                            { pos = from
                                            , path = pos :: path
                                            }
                                        )

                            Origin _ ->
                                Just
                                    { pos = pos
                                    , path = pos :: path
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
                    target.from
                        |> Dict.keys
                        |> List.map
                            (\relativePos ->
                                relativePos
                                    |> RelativePos.toDir level
                                    |> Dir.addTo pos
                            )

                maybeOriginId =
                    case
                        target.from
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
                                |> List.foldl
                                    (\_ ->
                                        stepThroughPath
                                            { level = level
                                            , grid = stage.grid
                                            , originId = target.originId
                                            }
                                    )
                                    (Just
                                        { pos = from
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
