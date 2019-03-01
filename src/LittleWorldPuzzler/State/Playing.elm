module LittleWorldPuzzler.State.Playing exposing (Model, Msg, init, update, view)

import Element exposing (Element)
import Framework.Modifier exposing (Modifier(..))
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import Http exposing (Error(..))
import LittleWorldPuzzler.Data.Board as Board
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Selected(..))
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry)
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.Request as Request exposing (Response(..))
import LittleWorldPuzzler.View.Game as GameView
import LittleWorldPuzzler.View.Header as HeaderView
import Process
import Random exposing (Generator, Seed)
import Task
import UndoList exposing (UndoList)



----------------------
-- Model
----------------------


type alias Basic =
    { game : Game
    }


type alias RunningState basic =
    { basic
        | selected : Maybe Selected
        , history : UndoList Game
    }


type alias EndState basic =
    { basic
        | history : UndoList Game
        , error : Maybe Error
    }


type alias LeaderboardState basic =
    { basic
        | highscore : Entry
        , newHighscore : Bool
    }


type State
    = Running (RunningState Basic)
    | Finished (EndState Basic)
    | Highscore (LeaderboardState Basic)


type alias Model =
    ( State, Seed )


type Msg
    = Selected Selected
    | PositionSelected Position
    | CardPlaced
    | RequestedReplay
    | RequestedHighscore Response



----------------------
-- Init
----------------------


stateGenerator : Generator State
stateGenerator =
    Game.generator
        |> Random.map
            (\game ->
                Running
                    { game = game
                    , selected = Nothing
                    , history = UndoList.fresh game
                    }
            )


init : Seed -> Model
init seed =
    Random.step stateGenerator seed



----------------------
-- Update
----------------------


play : (Model -> model) -> UndoList Game -> ( Game, Seed ) -> ( model, Cmd Msg )
play modelMapper history ( game, seed ) =
    let
        seconds : Float
        seconds =
            1000
    in
    ( modelMapper
        ( Running
            { game = game
            , selected = Nothing
            , history = history |> UndoList.new game
            }
        , seed
        )
    , Task.perform (always CardPlaced) <| Process.sleep (0.1 * seconds)
    )


playFirst : (Model -> model) -> Position -> Game -> UndoList Game -> Seed -> ( model, Cmd Msg )
playFirst modelMapper position ({ board } as game) history seed =
    Random.step
        (Deck.playFirst game.deck
            |> Random.map
                (\deck ->
                    { game
                        | deck = deck
                        , board =
                            board
                                |> Board.place position
                                    (game.deck |> Deck.first)
                    }
                )
        )
        seed
        |> play modelMapper history


playSecond : (Model -> model) -> Position -> CellType -> Game -> UndoList Game -> (Seed -> ( model, Cmd Msg ))
playSecond modelMapper position cellType ({ board, deck } as game) history =
    { game
        | deck = deck |> Deck.playSecond
        , board = board |> Board.place position cellType
    }
        |> (\a b ->
                ( a, b ) |> play modelMapper history
           )


update : (UndoList Game -> model) -> (Model -> model) -> Msg -> Model -> ( model, Cmd Msg )
update replayGame modelMapper msg ( state, seed ) =
    let
        defaultCase : ( model, Cmd Msg )
        defaultCase =
            ( modelMapper ( state, seed ), Cmd.none )
    in
    case msg of
        Selected select ->
            case state of
                Running runningState ->
                    ( modelMapper
                        ( Running { runningState | selected = Just select }
                        , seed
                        )
                    , Cmd.none
                    )

                _ ->
                    defaultCase

        PositionSelected position ->
            case state of
                Running { game, history, selected } ->
                    case selected of
                        Just First ->
                            playFirst modelMapper position game history seed

                        Just Second ->
                            case game.deck |> Deck.second of
                                Just second ->
                                    playSecond modelMapper position second game history seed

                                Nothing ->
                                    playFirst modelMapper position game history seed

                        Nothing ->
                            defaultCase

                _ ->
                    defaultCase

        CardPlaced ->
            case state of
                Running ({ game, history } as runningState) ->
                    let
                        newGame : Game
                        newGame =
                            game |> Game.step

                        newHistory : UndoList Game
                        newHistory =
                            history |> UndoList.new newGame
                    in
                    if newGame.board |> Grid.emptyPositions |> (==) [] then
                        ( modelMapper
                            ( Finished
                                { game = newGame
                                , history = newHistory
                                , error = Nothing
                                }
                            , seed
                            )
                        , Request.getHighscore game.score
                            |> Cmd.map RequestedHighscore
                        )

                    else
                        ( modelMapper
                            ( Running
                                { runningState
                                    | game = newGame
                                    , history = newHistory
                                }
                            , seed
                            )
                        , Cmd.none
                        )

                _ ->
                    defaultCase

        RequestedReplay ->
            case state of
                Highscore { highscore } ->
                    case highscore.history |> UndoList.toList of
                        present :: future ->
                            ( replayGame <| UndoList.fromList present future
                            , Cmd.none
                            )

                        _ ->
                            defaultCase

                _ ->
                    defaultCase

        RequestedHighscore response ->
            case state of
                Finished { history, game } ->
                    case response of
                        GotHighscore entry ->
                            ( modelMapper
                                ( Highscore
                                    { game = game
                                    , highscore = entry
                                    , newHighscore = False
                                    }
                                , seed
                                )
                            , Cmd.none
                            )

                        AchivedNewHighscore ->
                            let
                                newEntry : Entry
                                newEntry =
                                    Entry.new history
                            in
                            ( modelMapper
                                ( Highscore
                                    { game = game
                                    , highscore = newEntry
                                    , newHighscore = True
                                    }
                                , seed
                                )
                            , Request.setHighscore newEntry
                                |> Cmd.map RequestedHighscore
                            )

                        GotError _ ->
                            defaultCase

                        Done ->
                            defaultCase

                _ ->
                    defaultCase



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper ( state, _ ) =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        (case state of
            Running { game, selected } ->
                [ HeaderView.view scale restartMsg game.score
                , GameView.view
                    { scale = scale
                    , selected = selected
                    , restartMsg = restartMsg
                    , status = Nothing
                    , highscore = Nothing
                    }
                    (Just
                        { positionSelectedMsg = msgMapper << PositionSelected
                        , selectedMsg = msgMapper << Selected
                        , requestedReplayMsg = msgMapper RequestedReplay
                        }
                    )
                    game
                ]

            Finished { game } ->
                [ HeaderView.view scale restartMsg game.score
                , GameView.view
                    { scale = scale
                    , selected = Nothing
                    , restartMsg = restartMsg
                    , status = Just Lost
                    , highscore = Nothing
                    }
                    (Just
                        { positionSelectedMsg = msgMapper << PositionSelected
                        , selectedMsg = msgMapper << Selected
                        , requestedReplayMsg = msgMapper RequestedReplay
                        }
                    )
                    game
                ]

            Highscore { game, highscore, newHighscore } ->
                [ HeaderView.view scale restartMsg game.score
                , GameView.view
                    { scale = scale
                    , selected = Nothing
                    , restartMsg = restartMsg
                    , status =
                        if newHighscore then
                            Just NewHighscore

                        else
                            Just Lost
                    , highscore = Just highscore.history.present.score
                    }
                    (Just
                        { positionSelectedMsg = msgMapper << PositionSelected
                        , selectedMsg = msgMapper << Selected
                        , requestedReplayMsg = msgMapper RequestedReplay
                        }
                    )
                    game
                ]
        )
