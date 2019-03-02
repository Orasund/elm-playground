module LittleWorldPuzzler.State.Finished exposing (Model, Msg(..), init, update, view)

import Element exposing (Element)
import Http exposing (Error(..))
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry)
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.Request as Request exposing (Response(..))
import LittleWorldPuzzler.State as State exposing (Action(..))
import LittleWorldPuzzler.View.Game as GameView
import LittleWorldPuzzler.View.Header as HeaderView
import UndoList exposing (UndoList)



----------------------
-- Init
----------------------


init : { game : Game, history : UndoList Game } -> ( Model, Cmd Msg )
init { game, history } =
    ( End
        { game = game
        , history = history
        , error = Nothing
        }
    , Request.getHighscore game.score
        |> Cmd.map RequestedHighscore
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


update : Msg -> Model -> Action Model Msg (UndoList Game)
update msg model =
    let
        defaultCase : Action Model Msg (UndoList Game)
        defaultCase =
            Update
                ( model, Cmd.none )
    in
    case msg of
        RequestedReplay ->
            case model of
                Highscore { highscore } ->
                    case highscore.history |> UndoList.toList of
                        present :: future ->
                            Transition <|
                                UndoList.fromList present future

                        _ ->
                            defaultCase

                _ ->
                    defaultCase

        RequestedHighscore response ->
            case model of
                End ({ history, game } as endState) ->
                    case response of
                        GotHighscore entry ->
                            Update
                                ( Highscore
                                    { game = game
                                    , highscore = entry
                                    , newHighscore = False
                                    }
                                , Cmd.none
                                )

                        AchievedNewHighscore ->
                            let
                                newEntry : Entry
                                newEntry =
                                    Entry.new history
                            in
                            Update
                                ( Highscore
                                    { game = game
                                    , highscore = newEntry
                                    , newHighscore = True
                                    }
                                , Request.setHighscore newEntry
                                    |> Cmd.map RequestedHighscore
                                )

                        GotError error ->
                            Update
                                ( End
                                    { endState | error = Just error }
                                , Cmd.none
                                )

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
            End { game, error } ->
                [ HeaderView.view scale restartMsg game.score
                , GameView.viewFinished
                    { scale = scale
                    , status = Lost
                    , highscore = Nothing
                    , requestedReplayMsg = msgMapper RequestedReplay
                    , error =
                        error
                            |> Maybe.map
                                (\e ->
                                    case e of
                                        BadUrl string ->
                                            "BadUrl: " ++ string

                                        Timeout ->
                                            "Timeout"

                                        NetworkError ->
                                            "Network Error"

                                        BadStatus int ->
                                            "Response Status: " ++ String.fromInt int

                                        BadBody string ->
                                            string
                                )
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
                    , error = Nothing
                    }
                    game
                ]
        )
