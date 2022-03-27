module Zess.Data.Game exposing (Change(..), Game, init, isDangerous, move, valid)

import Dict exposing (Dict)
import List.Extra
import Process exposing (spawn)
import Random exposing (Generator)
import Random.List
import Zess.Config as Config
import Zess.Data.Figure as Figure exposing (Figure(..), FigureId)


type alias Game =
    { figures : Dict FigureId Figure
    , grid : Dict ( Int, Int ) FigureId
    , player : ( Int, Int )
    , next : ( List Figure, List (List Figure) )
    , score : Int
    , waveSize : Int
    }


type Change
    = Spawn FigureId ( Int, Int )
    | Move FigureId ( Int, Int )
    | Kill FigureId
    | GameOver


generateNextEnemies : Int -> Generator ( List Figure, List (List Figure) )
generateNextEnemies waveSize =
    Random.List.shuffle Figure.asList
        |> Random.map
            (\list ->
                list
                    |> List.Extra.groupsOf waveSize
                    |> List.Extra.uncons
                    |> Maybe.withDefault ( [], [] )
            )


init : Generator ( Game, List Change )
init =
    generateNextEnemies Config.initialWaveSize
        |> Random.map
            (\next ->
                ( { figures = Dict.empty
                  , grid =
                        --player does not have a figure
                        Dict.singleton Config.startingPos Config.playerId
                  , player = Config.startingPos
                  , next = next
                  , score = 0
                  , waveSize = Config.initialWaveSize
                  }
                , [ Spawn Config.playerId Config.startingPos ]
                )
            )
        |> andThen spawnEnemy


andThen : (Game -> Generator ( Game, List Change )) -> Generator ( Game, List Change ) -> Generator ( Game, List Change )
andThen fun generator =
    generator
        |> Random.andThen
            (\( game, changes ) ->
                game
                    |> fun
                    |> Random.map (Tuple.mapSecond (\newChanges -> changes ++ newChanges))
            )


isDangerous : ( Int, Int ) -> Game -> Bool
isDangerous ( x, y ) game =
    List.range 0 (Config.size - 1)
        |> List.concatMap
            (\i ->
                [ ( ( i, y ), [ Rook, Queen ] )
                , ( ( x, i ), [ Rook, Queen ] )
                , ( ( i, y - x + i ), [ Biship, Queen ] )
                , ( ( i, y + x - i ), [ Biship, Queen ] )
                ]
            )
        |> List.any
            (\( pos, figures ) ->
                (pos /= ( x, y ))
                    && (pos /= game.player)
                    && (game.grid
                            |> Dict.get pos
                            |> Maybe.andThen (\id -> game.figures |> Dict.get id)
                            |> (\maybe ->
                                    maybe
                                        |> Maybe.map (\figure -> figures |> List.member figure)
                                        |> Maybe.withDefault False
                               )
                       )
            )


canMove : { from : ( Int, Int ), to : ( Int, Int ) } -> Game -> Figure -> Bool
canMove args game figure =
    let
        from =
            { x = Tuple.first args.from
            , y = Tuple.second args.from
            }

        to =
            { x = Tuple.first args.to
            , y = Tuple.second args.to
            }
    in
    from
        /= to
        && (to.x >= 0 && to.x < Config.size)
        && (to.y >= 0)
        && (case figure of
                King ->
                    (abs (from.x - to.x) <= 1)
                        && (abs (from.y - to.y) <= 1)

                Rook ->
                    if from.x == to.x then
                        List.range (min from.y to.y) (max from.y to.y)
                            |> List.all
                                (\y ->
                                    (y == to.y)
                                        || (y == from.y)
                                        || (game.grid |> Dict.member ( from.x, y ) |> not)
                                )

                    else if from.y == to.y then
                        List.range (min from.x to.x) (max from.x to.x)
                            |> List.all
                                (\x ->
                                    (x == to.x)
                                        || (x == from.x)
                                        || (game.grid |> Dict.member ( x, from.y ) |> not)
                                )

                    else
                        False

                Biship ->
                    if abs (from.x - to.x) == abs (from.y - to.y) then
                        let
                            dir =
                                { x = to.x - from.x |> clamp -1 1, y = to.y - from.y |> clamp -1 1 }

                            n =
                                abs (from.x - to.x)
                        in
                        List.range 1 n
                            |> List.map (\i -> ( from.x + dir.x * i, from.y + dir.y * i ))
                            |> List.all (\pos -> game.grid |> Dict.member pos |> not)

                    else
                        False

                Knight ->
                    [ ( 2, 1 ), ( 1, 2 ), ( -1, 2 ), ( -2, 1 ), ( -2, -1 ), ( -1, -2 ), ( 1, -2 ), ( 2, -1 ) ]
                        |> List.member ( to.x - from.x, to.y - from.y )

                Pawn ->
                    if to.x == from.x && to.y == from.y + 1 then
                        game.player /= ( to.x, to.y )

                    else if abs (to.x - from.x) == 1 && to.y == from.y + 1 then
                        game.player == ( to.x, to.y )

                    else
                        False

                Queen ->
                    if from.x == to.x then
                        List.range (min from.y to.y) (max from.y to.y)
                            |> List.all
                                (\y ->
                                    (y == to.y)
                                        || (y == from.y)
                                        || (game.grid |> Dict.member ( from.x, y ) |> not)
                                )

                    else if from.y == to.y then
                        List.range (min from.x to.x) (max from.x to.x)
                            |> List.all
                                (\x ->
                                    (x == to.x)
                                        || (x == from.x)
                                        || (game.grid |> Dict.member ( x, from.y ) |> not)
                                )

                    else if abs (from.x - to.x) == abs (from.y - to.y) then
                        let
                            dir =
                                { x = to.x - from.x |> clamp -1 1, y = to.y - from.y |> clamp -1 1 }

                            n =
                                abs (from.x - to.x)
                        in
                        List.range 1 n
                            |> List.map (\i -> ( from.x + dir.x * i, from.y + dir.y * i ))
                            |> List.all (\pos -> game.grid |> Dict.member pos |> not)

                    else
                        False
           )


valid : { isEnemy : Bool, from : ( Int, Int ), to : ( Int, Int ) } -> Game -> Bool
valid args game =
    if args.from == args.to then
        False

    else
        case game.grid |> Dict.get args.from of
            Just figureId ->
                let
                    figureCanMove =
                        game.figures
                            |> Dict.get figureId
                            |> Maybe.withDefault Figure.player
                            |> canMove { from = args.from, to = args.to } game
                in
                figureCanMove
                    && (if args.isEnemy then
                            Dict.member args.to game.grid |> not

                        else
                            True
                       )

            Nothing ->
                False


spawnEnemy : Game -> Generator ( Game, List Change )
spawnEnemy game =
    case game.next of
        ( nextFigure :: nextRest, nextChunks ) ->
            List.range 0 (Config.size - 1)
                |> List.map (\x -> ( x, -1 ))
                |> List.filter (\pos -> (game.grid |> Dict.member pos |> not) && (pos /= game.player))
                |> List.Extra.uncons
                |> Maybe.map (\( head, tail ) -> Random.uniform head tail)
                |> Maybe.map
                    (Random.map
                        (\pos ->
                            let
                                newFigureId =
                                    game.figures |> Dict.size
                            in
                            ( { game
                                | figures = game.figures |> Dict.insert newFigureId nextFigure
                                , grid = game.grid |> Dict.insert pos newFigureId
                                , next = ( nextRest, nextChunks )
                              }
                            , [ Spawn newFigureId pos ]
                            )
                        )
                    )
                |> Maybe.withDefault (Random.constant ( game, [] ))

        ( [], nextChunks ) ->
            (case nextChunks of
                [] ->
                    generateNextEnemies (game.waveSize + 1)
                        |> Random.map (\it -> ( it, game.waveSize ))

                [ next ] ->
                    generateNextEnemies (game.waveSize + 1)
                        |> Random.map (\( head, tail ) -> ( ( next, head :: tail ), game.waveSize + 1 ))

                head :: tail ->
                    Random.constant ( ( head, tail ), game.waveSize )
            )
                |> Random.map
                    (Tuple.mapFirst
                        (\( head, tail ) ->
                            if Dict.size game.grid <= 2 then
                                ( head, tail )

                            else
                                ( [], head :: tail )
                        )
                    )
                |> Random.map (\( next, waveSize ) -> ( { game | next = next, waveSize = waveSize }, [] ))


moveEnemy : ( ( Int, Int ), FigureId ) -> Game -> Generator ( Game, List Change )
moveEnemy ( pos, figureId ) game =
    let
        moveTo : ( Int, Int ) -> ( Game, List Change )
        moveTo ( x, y ) =
            if y < Config.size then
                ( { game
                    | grid =
                        game.grid
                            |> Dict.remove pos
                            |> Dict.insert ( x, y ) figureId
                  }
                , [ Move figureId ( x, y ) ]
                )

            else
                ( { game
                    | grid =
                        game.grid
                            |> Dict.remove pos
                  }
                , [ Move figureId ( x, y ), Kill figureId ]
                )
    in
    if valid { isEnemy = True, from = pos, to = game.player } game then
        moveTo game.player |> Random.constant

    else
        case
            game.figures
                |> Dict.get figureId
                |> Maybe.withDefault Figure.player
                |> Figure.moves pos
                |> List.filter (\to -> valid { isEnemy = True, from = pos, to = to } game)
        of
            head :: tail ->
                Random.uniform head tail
                    |> Random.map moveTo

            [] ->
                Random.constant ( game, [] )


move : ( Int, Int ) -> Game -> Generator (Maybe ( Game, List Change ))
move pos game =
    let
        enemies =
            game.grid
                |> Dict.remove game.player
                |> Dict.remove pos
    in
    if valid { isEnemy = False, from = game.player, to = pos } game then
        enemies
            |> Dict.toList
            |> Random.List.shuffle
            |> Random.andThen
                (List.foldl (\tuple -> andThen (moveEnemy tuple))
                    (Random.constant
                        ( { game
                            | grid = enemies
                            , player = pos
                            , score =
                                game.score
                                    + (game.grid
                                        |> Dict.get pos
                                        |> Maybe.andThen (\id -> game.figures |> Dict.get id)
                                        |> Maybe.map Figure.score
                                        |> Maybe.withDefault 0
                                      )
                          }
                        , Move Config.playerId pos
                            :: (game.grid
                                    |> Dict.get pos
                                    |> Maybe.map (\id -> [ Kill id ])
                                    |> Maybe.withDefault []
                               )
                        )
                    )
                )
            |> andThen spawnEnemy
            |> andThen
                (\g ->
                    Random.constant
                        ( { g
                            | grid =
                                g.grid
                                    |> Dict.update pos (\maybe -> maybe |> Maybe.withDefault Config.playerId |> Just)
                          }
                        , if g.grid |> Dict.member pos then
                            [ Kill Config.playerId
                            , GameOver
                            ]

                          else
                            []
                        )
                )
            |> Random.map Just

    else
        Random.constant Nothing
