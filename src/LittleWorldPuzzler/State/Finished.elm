module LittleWorldPuzzler.State.Finished exposing (Model, Msg(..), init, update, view)

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
-- Init
----------------------


init : (Model -> model) -> (Msg -> msg) -> Game -> UndoList Game -> ( model, Cmd msg )
init modelMapper msgMapper game history =
    ( modelMapper <|
        End
            { game = game
            , history = history
            , error = Nothing
            }
    , Request.getHighscore game.score
        |> Cmd.map (msgMapper << RequestedHighscore)
    )



----------------------
-- Model
----------------------


type alias Basic =
    { game : Game
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


type Model
    = End (EndState Basic)
    | Highscore (LeaderboardState Basic)


type Msg
    = RequestedReplay
    | RequestedHighscore Response



----------------------
-- Update
----------------------


update : (UndoList Game -> model) -> (Model -> model) -> Msg -> Model -> ( model, Cmd Msg )
update replayGame modelMapper msg model =
    let
        defaultCase : ( model, Cmd Msg )
        defaultCase =
            ( modelMapper model, Cmd.none )
    in
    case msg of
        RequestedReplay ->
            case model of
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
            case model of
                End { history, game } ->
                    case response of
                        GotHighscore entry ->
                            ( modelMapper <|
                                Highscore
                                    { game = game
                                    , highscore = entry
                                    , newHighscore = False
                                    }
                            , Cmd.none
                            )

                        AchivedNewHighscore ->
                            let
                                newEntry : Entry
                                newEntry =
                                    Entry.new history
                            in
                            ( modelMapper <|
                                Highscore
                                    { game = game
                                    , highscore = newEntry
                                    , newHighscore = True
                                    }
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
view scale restartMsg msgMapper model =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        (case model of
            End { game } ->
                [ HeaderView.view scale restartMsg game.score
                , GameView.viewFinished
                    { scale = scale
                    , status = Lost
                    , highscore = Nothing
                    , requestedReplayMsg = msgMapper RequestedReplay
                    }
                    game
                ]

            Highscore { game, highscore, newHighscore } ->
                [ HeaderView.view scale restartMsg game.score
                , GameView.viewFinished
                    { scale = scale
                    , status =
                        if newHighscore then
                            NewHighscore

                        else
                            Lost
                    , highscore = Just highscore.history.present.score
                    , requestedReplayMsg = msgMapper RequestedReplay
                    }
                    game
                ]
        )
