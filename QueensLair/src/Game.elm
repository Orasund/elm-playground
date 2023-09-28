module Game exposing (..)

import Config
import Dict exposing (Dict)
import MinimaxSearch exposing (Evaluation(..))
import Piece exposing (Piece(..))
import Set exposing (Set)
import Square exposing (Square)


type alias Game =
    { board : Dict ( Int, Int ) Square }


new : Game
new =
    { board =
        [ ( ( 2, 0 ), { isWhite = False, piece = King } )
        , ( ( 2, 3 ), { isWhite = True, piece = King } )
        ]
            |> Dict.fromList
    }


isValidMove : { from : ( Int, Int ), to : ( Int, Int ) } -> Game -> Bool
isValidMove args game =
    let
        isValidPos ( i1, i2 ) =
            (0 <= i1)
                && (i1 < Config.boardSize)
                && (0 <= i2)
                && (i2 < Config.boardSize)

        canCapture square =
            game.board
                |> Dict.get args.to
                |> Maybe.map (\{ isWhite } -> isWhite /= square.isWhite)
                |> Maybe.withDefault True

        ( fromX, fromY ) =
            args.from

        ( toX, toY ) =
            args.to
    in
    isValidPos args.from
        && isValidPos args.to
        && (game.board
                |> Dict.get args.from
                |> Maybe.map
                    (\square ->
                        canCapture square
                            && (Piece.movement square.piece
                                    |> Set.member ( toX - fromX, toY - fromY )
                               )
                    )
                |> Maybe.withDefault False
           )


possibleMoves : ( Int, Int ) -> Game -> Set ( Int, Int )
possibleMoves ( x, y ) game =
    game.board
        |> Dict.get ( x, y )
        |> Maybe.map
            (\square ->
                square.piece
                    |> Piece.movement
                    |> Set.map
                        (\( relX, relY ) ->
                            ( x + relX, y + relY )
                        )
                    |> Set.filter
                        (\to ->
                            isValidMove
                                { from = ( x, y )
                                , to = to
                                }
                                game
                        )
            )
        |> Maybe.withDefault Set.empty


findNextMove : Game -> Maybe { from : ( Int, Int ), to : ( Int, Int ) }
findNextMove game =
    game
        |> MinimaxSearch.findBestMove
            { apply = move
            , evaluate = evaluateForBlack
            , possibleMoves =
                \{ isYourTurn } g ->
                    if isWon g || isLost g then
                        []

                    else
                        g.board
                            |> Dict.filter (\_ square -> not square.isWhite == isYourTurn)
                            |> Dict.keys
                            |> List.concatMap
                                (\pos ->
                                    possibleMoves pos g
                                        |> Set.toList
                                        |> List.map
                                            (\to ->
                                                { from = pos, to = to }
                                            )
                                )
            , searchDepth = 5
            }


isWon : Game -> Bool
isWon game =
    (game.board
        |> Dict.filter
            (\( _, y ) square ->
                (square.piece == King)
                    && square.isWhite
                    && (y == 0)
            )
        |> Dict.isEmpty
        |> not
    )
        || (game.board
                |> Dict.filter (\_ square -> not square.isWhite)
                |> Dict.isEmpty
           )


isLost : Game -> Bool
isLost game =
    game.board
        |> Dict.filter
            (\( _, y ) square ->
                (square.piece == King)
                    && square.isWhite
            )
        |> Dict.isEmpty


evaluateForBlack : Game -> Evaluation
evaluateForBlack game =
    evaluateForWhite game |> MinimaxSearch.negateEvaluation


evaluateForWhite : Game -> Evaluation
evaluateForWhite game =
    if isWon game then
        Winning

    else if isLost game then
        Loosing

    else
        game.board
            |> Dict.values
            |> List.foldl
                (\square score ->
                    case score of
                        Score n ->
                            n
                                + Piece.value square.piece
                                * (if square.isWhite then
                                    1

                                   else
                                    -1
                                  )
                                |> Score

                        _ ->
                            score
                )
                (Score 0)


move : { from : ( Int, Int ), to : ( Int, Int ) } -> Game -> Game
move args game =
    game.board
        |> Dict.get args.from
        |> Maybe.map
            (\square ->
                game.board
                    |> Dict.remove args.from
                    |> Dict.insert args.to square
            )
        |> Maybe.withDefault game.board
        |> (\board -> { game | board = board })
