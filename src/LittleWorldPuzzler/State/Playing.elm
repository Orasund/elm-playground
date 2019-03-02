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
import LittleWorldPuzzler.State as State exposing (Action(..))
import LittleWorldPuzzler.View.Game as GameView
import LittleWorldPuzzler.View.Header as HeaderView
import Process
import Random exposing (Generator, Seed)
import Task
import UndoList exposing (UndoList)



----------------------
-- Model
----------------------


type alias State =
    { game : Game
    , selected : Maybe Selected
    , history : UndoList Game
    , trainingMode : Bool
    }


type alias Model =
    ( State, Seed )


type Msg
    = Selected Selected
    | PositionSelected Position
    | CardPlaced
    | Undo
    | Redo



----------------------
-- Init
----------------------


init : { game : Game, seed : Seed, trainingMode : Bool } -> ( Model, Cmd Msg )
init { game, seed, trainingMode } =
    ( ( { game = game
        , selected = Nothing
        , history = UndoList.fresh game
        , trainingMode = trainingMode
        }
      , seed
      )
    , Cmd.none
    )



----------------------
-- Update
----------------------


play : Model -> Action Model Msg { game : Game, history : UndoList Game }
play ( { game, history } as state, seed ) =
    let
        seconds : Float
        seconds =
            1000
    in
    Update
        ( ( { state
                | game = game
                , selected = Nothing
                , history = history |> UndoList.new game
            }
          , seed
          )
        , Task.perform (always CardPlaced) <| Process.sleep (0.1 * seconds)
        )


playFirst : Position -> Model -> Action Model Msg { game : Game, history : UndoList Game }
playFirst position ( { game, history } as state, seed ) =
    Random.step
        (Deck.playFirst game.deck
            |> Random.map
                (\deck ->
                    { state
                        | game =
                            { game
                                | deck = deck
                                , board =
                                    game.board
                                        |> Board.place position
                                            (game.deck |> Deck.first)
                            }
                    }
                )
        )
        seed
        |> play


playSecond : Position -> CellType -> Model -> Action Model Msg { game : Game, history : UndoList Game }
playSecond position cellType ( { game } as state, seed ) =
    play
        ( { state
            | game =
                { game
                    | deck = game.deck |> Deck.playSecond
                    , board = game.board |> Board.place position cellType
                }
          }
        , seed
        )


update : Msg -> Model -> Action Model Msg { game : Game, history : UndoList Game }
update msg (( { game, history, selected, trainingMode } as state, seed ) as model) =
    let
        defaultCase : Action Model Msg { game : Game, history : UndoList Game }
        defaultCase =
            Update
                ( model, Cmd.none )
    in
    case msg of
        Selected select ->
            Update
                ( ( { state | selected = Just select }
                  , seed
                  )
                , Cmd.none
                )

        PositionSelected position ->
            case selected of
                Just First ->
                    playFirst position model

                Just Second ->
                    case game.deck |> Deck.second of
                        Just second ->
                            playSecond position second model

                        Nothing ->
                            playFirst position model

                Nothing ->
                    defaultCase

        CardPlaced ->
            let
                newGame : Game
                newGame =
                    game |> Game.step

                newHistory : UndoList Game
                newHistory =
                    history |> UndoList.new newGame
            in
            if (newGame.board |> Grid.emptyPositions |> (==) []) && not trainingMode then
                Transition
                    { game = newGame
                    , history = newHistory
                    }

            else
                Update
                    ( ( { state
                            | game = newGame
                            , history = newHistory
                        }
                      , seed
                      )
                    , Cmd.none
                    )

        Redo ->
            let
                newHistory : UndoList Game
                newHistory =
                    history
                        |> UndoList.redo
                        |> UndoList.redo
            in
            Update
                ( ( { state
                        | history = newHistory
                        , game = newHistory |> .present
                    }
                  , seed
                  )
                , Cmd.none
                )

        Undo ->
            let
                newHistory : UndoList Game
                newHistory =
                    history
                        |> UndoList.undo
                        |> UndoList.undo
            in
            Update
                ( ( { state
                        | history = newHistory
                        , game = newHistory |> .present
                    }
                  , seed
                  )
                , Cmd.none
                )



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper ( { game, selected, trainingMode }, _ ) =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ if trainingMode then
            HeaderView.viewWithUndo scale
                { restartMsg = restartMsg
                , previousMsg = msgMapper Undo
                , nextMsg = msgMapper Redo
                }
                game.score

          else
            HeaderView.view scale restartMsg game.score
        , GameView.view
            { scale = scale
            , selected = selected
            }
            { positionSelectedMsg = msgMapper << PositionSelected
            , selectedMsg = msgMapper << Selected
            }
            game
        ]
