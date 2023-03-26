module Game exposing (..)

import Action exposing (Action)
import Config
import Dict exposing (Dict)
import Random exposing (Generator)
import Sound
import Tile exposing (Tile, TileId)
import World


type alias Game =
    { tiles : Dict TileId Tile
    , nextTileId : TileId
    , world : Dict Int TileId
    , playerPos : Int
    }


init : Game
init =
    { tiles =
        [ ( 0, Tile.Player )
        , ( 1, Tile.Door )
        , ( 2, Tile.Door )
        ]
            |> Dict.fromList
    , nextTileId = 3
    , world =
        Dict.empty
            |> Dict.insert 0 0
            |> Dict.insert Config.maxDistance 1
            |> Dict.insert -Config.maxDistance 2
    , playerPos = 0
    }


newLevel : Int -> Generator Game
newLevel level =
    World.generate level
        |> Random.map
            (\world ->
                { tiles =
                    world
                        |> Dict.toList
                        |> List.indexedMap (\i ( _, tile ) -> ( i, tile ))
                        |> Dict.fromList
                , nextTileId = world |> Dict.size
                , world =
                    world
                        |> Dict.toList
                        |> List.indexedMap (\i ( pos, _ ) -> ( pos, i ))
                        |> Dict.fromList
                , playerPos = 0
                }
            )


nextTileLeftOf : Int -> Game -> Maybe ( Int, Tile )
nextTileLeftOf pos game =
    if pos < -Config.maxDistance then
        Nothing

    else
        case game.world |> Dict.get (pos - 1) of
            Just tileId ->
                game.tiles
                    |> Dict.get tileId
                    |> Maybe.map
                        (\tile ->
                            ( pos - 1, tile )
                        )

            Nothing ->
                nextTileLeftOf (pos - 1) game


nextTileRightOf : Int -> Game -> Maybe ( Int, Tile )
nextTileRightOf pos game =
    if pos > Config.maxDistance then
        Nothing

    else
        case game.world |> Dict.get (pos + 1) of
            Just tileId ->
                game.tiles
                    |> Dict.get tileId
                    |> Maybe.map
                        (\tile ->
                            ( pos + 1, tile )
                        )

            Nothing ->
                nextTileRightOf (pos + 1) game


moveTile : { from : Int, to : Int } -> Game -> Maybe Game
moveTile args game =
    game.world
        |> Dict.get args.from
        |> Maybe.map
            (\tile ->
                game.world
                    |> Dict.remove args.from
                    |> Dict.insert args.to tile
                    |> (\world -> { game | world = world })
            )


spawnTile : Int -> Tile -> Game -> Game
spawnTile pos tile game =
    { game
        | world =
            game.world
                |> Dict.insert pos game.nextTileId
        , tiles = game.tiles |> Dict.insert game.nextTileId tile
        , nextTileId = game.nextTileId + 1
    }


tick : Game -> List Action
tick game =
    game.world
        |> Dict.toList
        |> List.filterMap
            (\( pos, tileId ) ->
                game.tiles
                    |> Dict.get tileId
                    |> Maybe.map
                        (\tile ->
                            ( pos, ( tileId, tile ) )
                        )
            )
        |> List.concatMap
            (\( pos, ( tileId, tile ) ) ->
                let
                    to =
                        if pos < game.playerPos then
                            pos + 1

                        else
                            pos - 1
                in
                case tile of
                    Tile.Enemy ->
                        game.world
                            |> Dict.get to
                            |> Maybe.andThen
                                (\id ->
                                    game.tiles |> Dict.get id
                                )
                            |> Maybe.map
                                (\t ->
                                    case t of
                                        Tile.Player ->
                                            [ Action.Move { from = pos, to = to }
                                            , Action.LooseLife { killedBy = tile }
                                            ]

                                        _ ->
                                            []
                                )
                            |> Maybe.withDefault
                                [ Action.Move { from = pos, to = to } ]

                    Tile.AxeThrower ->
                        game.world
                            |> Dict.get to
                            |> Maybe.andThen
                                (\id ->
                                    game.tiles |> Dict.get id
                                )
                            |> Maybe.map
                                (\t ->
                                    case t of
                                        Tile.Player ->
                                            [ Action.Move { from = pos, to = to }
                                            , Action.LooseLife { killedBy = tile }
                                            ]

                                        Tile.Chair ->
                                            [ Action.Kill to ]

                                        _ ->
                                            []
                                )
                            |> Maybe.withDefault
                                [ Action.Spawn to Tile.Axe ]

                    Tile.Axe ->
                        (if pos < game.playerPos then
                            nextTileRightOf pos game

                         else
                            nextTileLeftOf pos game
                        )
                            |> Maybe.map
                                (\( p, t ) ->
                                    case t of
                                        Tile.Player ->
                                            [ Action.Move { from = pos, to = p }
                                            , Action.LooseLife { killedBy = tile }
                                            ]

                                        Tile.Chair ->
                                            [ Action.Move { from = pos, to = p }
                                            , Action.Kill p
                                            ]

                                        Tile.Shield ->
                                            (if pos < game.playerPos then
                                                p - 1

                                             else
                                                p + 1
                                            )
                                                |> (\it ->
                                                        [ Action.Move { from = pos, to = it }
                                                        , Action.Kill it
                                                        ]
                                                   )

                                        Tile.Door ->
                                            []

                                        _ ->
                                            [ Action.Move { from = pos, to = p } ]
                                )
                            |> Maybe.withDefault
                                [ Action.Move { from = pos, to = to } ]

                    Tile.Player ->
                        []

                    Tile.Door ->
                        []

                    Tile.Chair ->
                        []

                    Tile.Shield ->
                        []

                    Tile.Money ->
                        []
            )


killTile : Int -> Game -> ( Game, List Action )
killTile pos game =
    ( { game
        | world =
            game.world
                |> Dict.update pos
                    (\maybe ->
                        maybe
                            |> Maybe.andThen
                                (\tileId ->
                                    case game.tiles |> Dict.get tileId of
                                        Just Tile.Door ->
                                            Just tileId

                                        _ ->
                                            Nothing
                                )
                    )
      }
    , game.world
        |> Dict.get pos
        |> Maybe.andThen (\tileId -> game.tiles |> Dict.get tileId)
        |> Maybe.map
            (\t ->
                case t of
                    Tile.Enemy ->
                        [ Action.AddPoint ]

                    Tile.AxeThrower ->
                        [ Action.AddPoint ]

                    _ ->
                        []
            )
        |> Maybe.withDefault []
    )


move : { left : Bool } -> Game -> ( Game, List Action )
move args game =
    (if args.left then
        nextTileLeftOf game.playerPos game

     else
        nextTileRightOf game.playerPos game
    )
        |> Maybe.map
            (\( pos, tile ) ->
                ( { game
                    | playerPos = pos
                  }
                , [ [ Action.Kill pos
                    , Action.PlaySound Sound.Punch
                    , Action.Move { from = game.playerPos, to = pos }
                    ]
                        |> Action.Chain
                        |> List.singleton
                  , case tile of
                        Tile.Enemy ->
                            []

                        Tile.Player ->
                            []

                        Tile.Door ->
                            [ Action.PlaySound Sound.Smash
                            , Action.LevelCleared
                            ]
                                |> Action.Chain
                                |> List.singleton

                        Tile.Chair ->
                            (if args.left then
                                pos - 1

                             else
                                pos + 1
                            )
                                |> (\it ->
                                        [ Action.Kill it ]
                                   )

                        Tile.AxeThrower ->
                            []

                        Tile.Axe ->
                            [ (if args.left then
                                pos - 1

                               else
                                pos + 1
                              )
                                |> Action.Kill
                            ]

                        Tile.Shield ->
                            [ Action.Spawn
                                (if args.left then
                                    pos + 1

                                 else
                                    pos - 1
                                )
                                Tile.Shield
                            ]

                        Tile.Money ->
                            [ Action.AddPoint
                            ]
                  , [ Action.Tick ]
                  ]
                    |> List.concat
                )
            )
        |> Maybe.withDefault ( game, [] )
