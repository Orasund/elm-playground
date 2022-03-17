module Ruz.Data.Game exposing (Game, init, isDangerous, move, valid)

import Dict exposing (Dict)
import List.Extra
import Process exposing (spawn)
import Random exposing (Generator)
import Random.List
import Ruz.Config as Config
import Ruz.Data.Figure as Figure exposing (Figure(..))


type alias Game =
    { figures : Dict Int Figure
    , grid : Dict ( Int, Int ) Int
    , player : ( Int, Int )
    , next : ( Figure, List Figure )
    , score : Int
    }


init : Generator Game
init =
    Random.List.shuffle Figure.asList
        |> Random.map
            (\next ->
                { figures = Dict.empty
                , grid =
                    --player does not have a figure
                    Dict.singleton Config.startingPos Config.playerId
                , player = Config.startingPos
                , next = next |> List.Extra.uncons |> Maybe.withDefault ( Figure.player, [] )
                , score = 0
                }
            )
        |> Random.andThen spawnEnemy


isDangerous : ( Int, Int ) -> Game -> Bool
isDangerous ( x, y ) game =
    List.range 0 (Config.size - 1)
        |> List.concatMap
            (\i ->
                [ ( ( i, y ), Rook )
                , ( ( x, i ), Rook )
                , ( ( i, y - x + i ), Biship )
                , ( ( i, y + x - i ), Biship )
                ]
            )
        |> List.any
            (\( pos, figure ) ->
                (pos /= ( x, y ))
                    && (pos /= game.player)
                    && (game.grid
                            |> Dict.get pos
                            |> Maybe.andThen (\id -> game.figures |> Dict.get id)
                            |> (==) (Just figure)
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


spawnEnemy : Game -> Generator Game
spawnEnemy game =
    let
        ( nextFigure, nextRest ) =
            game.next
    in
    List.range 0 (Config.size - 1)
        |> List.map (\x -> ( x, -1 ))
        |> List.filter (\pos -> (game.grid |> Dict.member pos |> not) && (pos /= game.player))
        |> List.Extra.uncons
        |> Maybe.map (\( head, tail ) -> Random.uniform head tail)
        |> Maybe.map
            (Random.andThen
                (\pos ->
                    (if nextRest == [] then
                        Random.List.shuffle Figure.asList

                     else
                        Random.constant nextRest
                    )
                        |> Random.map
                            (\list ->
                                let
                                    newFigureId =
                                        game.figures |> Dict.size
                                in
                                { game
                                    | figures = game.figures |> Dict.insert newFigureId nextFigure
                                    , grid = game.grid |> Dict.insert pos newFigureId
                                    , next = list |> List.Extra.uncons |> Maybe.withDefault ( Figure.player, [] )
                                }
                            )
                )
            )
        |> Maybe.withDefault (Random.constant game)


moveEnemy : ( ( Int, Int ), Int ) -> Game -> Generator Game
moveEnemy ( pos, figureId ) game =
    let
        moveTo to =
            { game
                | grid =
                    game.grid
                        |> Dict.remove pos
                        |> Dict.insert to figureId
            }
    in
    if valid { isEnemy = True, from = pos, to = game.player } game then
        Random.constant (moveTo game.player)

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
                Random.constant game


move : ( Int, Int ) -> Game -> Generator (Maybe { gameOver : Bool, game : Game })
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
                (List.foldl (\tuple -> Random.andThen (moveEnemy tuple))
                    (Random.constant
                        { game
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
                    )
                )
            |> Random.andThen spawnEnemy
            |> Random.map
                (\g ->
                    { gameOver = g.grid |> Dict.member pos
                    , game =
                        { g
                            | grid =
                                g.grid
                                    |> Dict.update pos (\maybe -> maybe |> Maybe.withDefault Config.playerId |> Just)
                                    |> Dict.filter (\( x, y ) _ -> y <= Config.size)
                        }
                    }
                        |> Just
                )

    else
        Random.constant Nothing
