module Game exposing (..)

import Config
import Dict exposing (Dict)
import MinimaxSearch exposing (Evaluation(..))
import Piece exposing (Piece(..))
import Set exposing (Set)
import Square exposing (Square)


type alias Game =
    { board : Dict ( Int, Int ) Square
    , history : List (Dict ( Int, Int ) Square)
    }


new : Game
new =
    {--[ ( ( 2, 3 ), { isWhite = True, piece = King } )
    , ( ( 1, 0 ), { isWhite = False, piece = Pawn } )
    , ( ( 2, 0 ), { isWhite = False, piece = Pawn } )
    , ( ( 0, 0 ), { isWhite = False, piece = Pawn } )
    , ( ( 3, 0 ), { isWhite = False, piece = Pawn } )
    ]--}
    fromPieces
        { white = [ King ], black = [ King ] }


fromPieces : { white : List Piece, black : List Piece } -> Game
fromPieces args =
    let
        white =
            args.white
                |> List.sortBy (\piece -> Piece.value piece |> negate)
                |> List.map2
                    (\pos piece ->
                        ( pos, { isWhite = True, piece = piece } )
                    )
                    [ ( 2, 3 )
                    , ( 1, 3 )
                    , ( 3, 3 )
                    , ( 0, 3 )
                    , ( 2, 2 )
                    , ( 1, 2 )
                    , ( 3, 2 )
                    , ( 0, 2 )
                    ]

        black =
            args.black
                |> List.sortBy (\piece -> Piece.value piece |> negate)
                |> List.map2
                    (\pos piece ->
                        ( pos, { isWhite = False, piece = piece } )
                    )
                    [ ( 2, 0 )
                    , ( 1, 0 )
                    , ( 3, 0 )
                    , ( 0, 0 )
                    , ( 2, 1 )
                    , ( 1, 1 )
                    , ( 3, 1 )
                    , ( 0, 1 )
                    ]
    in
    white
        ++ black
        |> Dict.fromList
        |> fromBoard


fromBoard : Dict ( Int, Int ) Square -> Game
fromBoard board =
    { board = board, history = [] }


isSave : { isWhite : Bool, pos : ( Int, Int ) } -> Game -> Bool
isSave args game =
    game.board
        |> Dict.filter (\_ square -> square.isWhite /= args.isWhite)
        |> Dict.keys
        |> List.any
            (\from ->
                isValidMove { from = from, to = args.pos } game
            )
        |> not


isValidMove : { from : ( Int, Int ), to : ( Int, Int ) } -> Game -> Bool
isValidMove args game =
    let
        isValidPos ( i1, i2 ) =
            (0 <= i1)
                && (i1 < Config.boardSize)
                && (0 <= i2)
                && (i2 < Config.boardSize)

        targetSquare =
            game.board
                |> Dict.get args.to

        canCapture square =
            targetSquare
                |> Maybe.map (\{ isWhite } -> isWhite /= square.isWhite)
                |> Maybe.withDefault True

        ( fromX, fromY ) =
            args.from

        ( toX, toY ) =
            args.to

        ( relX, relY ) =
            ( toX - fromX, toY - fromY )

        sign x =
            if x > 0 then
                1

            else if x < 0 then
                -1

            else
                0

        specialCase square =
            case square.piece of
                King ->
                    not square.isWhite
                        || (toY /= 0)
                        || isSave { isWhite = True, pos = args.to }
                            (move { from = args.from, to = args.to } game)

                Rook ->
                    List.range 1 (abs relX + abs relY - 1)
                        |> List.all
                            (\i ->
                                game.board
                                    |> Dict.get ( fromX + i * sign relX, fromY + i * sign relY )
                                    |> (==) Nothing
                            )

                Bishop ->
                    List.range 1 (abs relX - 1)
                        |> List.all
                            (\i ->
                                game.board
                                    |> Dict.get ( fromX + i * sign relX, fromY + i * sign relY )
                                    |> (==) Nothing
                            )

                Knight ->
                    True

                Pawn ->
                    if relY == -1 then
                        square.isWhite
                            && (if relX == 0 then
                                    targetSquare == Nothing

                                else
                                    targetSquare
                                        |> Maybe.map .isWhite
                                        |> (==) (Just False)
                               )

                    else
                        not square.isWhite
                            && (if relX == 0 then
                                    targetSquare == Nothing

                                else
                                    targetSquare
                                        |> Maybe.map .isWhite
                                        |> (==) (Just True)
                               )
    in
    isValidPos args.from
        && isValidPos args.to
        --&& (move { from = args.from, to = args.to } game |> (\g -> List.member g.board g.history))
        && (game.board
                |> Dict.get args.from
                |> Maybe.map
                    (\square ->
                        canCapture square
                            && (Piece.movement square.piece
                                    |> Set.member ( relX, relY )
                               )
                            && specialCase square
                    )
                |> Maybe.withDefault False
           )


possibleMovesFor : ( Int, Int ) -> Game -> Set ( Int, Int )
possibleMovesFor ( x, y ) game =
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
            , possibleMoves = possibleMoves
            , searchDepth = 5
            }


possibleMoves : { isYourTurn : Bool } -> Game -> List { from : ( Int, Int ), to : ( Int, Int ) }
possibleMoves args game =
    if isWon game || isLost game then
        []

    else
        game.board
            |> Dict.filter (\_ square -> not square.isWhite == args.isYourTurn)
            |> Dict.keys
            |> List.concatMap
                (\pos ->
                    possibleMovesFor pos game
                        |> Set.toList
                        |> List.map
                            (\to ->
                                { from = pos, to = to }
                            )
                )


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
            (\( _, _ ) square ->
                (square.piece == King)
                    && square.isWhite
            )
        |> Dict.isEmpty



--     || List.member game.board game.history


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
            |> Dict.toList
            |> List.foldl
                (\( _, square ) score ->
                    case score of
                        Score n ->
                            n
                                + {--((possibleMoves ( x, y ) game |> Set.size) + Piece.value square.piece)
                                * (if square.isWhite then
                                    1

                                   else
                                    -1
                                  )--}
                                  (if square.isWhite then
                                    case square.piece of
                                        King ->
                                            --(Config.boardSize - y) *
                                            Piece.value square.piece

                                        _ ->
                                            Piece.value square.piece

                                   else
                                    case square.piece of
                                        Pawn ->
                                            -(Piece.value square.piece)

                                        _ ->
                                            -(Piece.value square.piece)
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
        |> (\board ->
                { game
                    | board = board
                    , history = game.board :: game.history
                }
           )
