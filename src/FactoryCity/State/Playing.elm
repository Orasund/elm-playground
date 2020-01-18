module FactoryCity.State.Playing exposing (Mode(..), Model, Msg, TransitionData, init, update, view)

import Action
import Element exposing (Element)
import FactoryCity.Data.Board as Board
import FactoryCity.Data.CellType exposing (CellType)
import FactoryCity.Data.Deck as Deck exposing (Selected(..))
import FactoryCity.Data.Game as Game exposing (EndCondition(..), Game)
import FactoryCity.State.Finished as FinishedState
import FactoryCity.View.Game as GameView
import FactoryCity.View.Header as HeaderView
import FactoryCity.View.PageSelector as PageSelectorView
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import Http exposing (Error(..))
import Process
import Random exposing (Seed)
import Set exposing (Set)
import Task
import UndoList exposing (UndoList)



----------------------
-- Model
----------------------


type Mode
    = Normal
    | Training
    | Challenge


type alias State =
    { game : Game
    , selected : Maybe Selected
    , history : UndoList Game
    , mode : Mode
    , collection : Set ( String, String )
    , viewCollection : Bool
    , viewedCard : Maybe CellType
    , initialSeed : Seed
    }


type alias Model =
    ( State, Seed )


type Msg
    = Selected Selected
    | PositionSelected Position
    | CardPlaced
    | Undo
    | Redo
    | PageChangeRequested
    | CardSelected CellType


type alias TransitionData =
    { game : Game
    , seed : Seed
    , mode : Mode
    }


type alias Action =
    Action.Action Model Msg FinishedState.TransitionData ()



----------------------
-- Init
----------------------


init : TransitionData -> ( Model, Cmd Msg )
init { game, seed, mode } =
    ( ( { game = game
        , selected = Nothing
        , history = UndoList.fresh game
        , mode = mode
        , collection = Set.empty
        , viewCollection = False
        , viewedCard = Nothing
        , initialSeed = seed
        }
      , seed
      )
    , Cmd.none
    )



----------------------
-- Update
----------------------


play : Model -> Action
play ( { game, history } as state, seed ) =
    let
        seconds : Float
        seconds =
            1000
    in
    Action.updating
        ( ( { state
                | game = game
                , selected = Nothing
                , history = history |> UndoList.new game
            }
          , seed
          )
        , Task.perform (always CardPlaced) <| Process.sleep (0.1 * seconds)
        )


playFirst : Position -> Model -> Action
playFirst position ( { game, mode, initialSeed } as state, seed ) =
    Random.step
        (Deck.playFirst { shuffle = mode /= Challenge } game.deck
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
        (if mode == Challenge then
            initialSeed

         else
            seed
        )
        |> play


playSecond : Position -> CellType -> Model -> Action
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


update : Msg -> Model -> Action
update msg (( { game, history, selected, mode, viewCollection, collection } as state, seed ) as model) =
    let
        defaultCase : Action
        defaultCase =
            Action.updating
                ( model, Cmd.none )
    in
    case msg of
        Selected select ->
            Action.updating
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
                ( newGame, newCollection ) =
                    game |> Game.step collection

                newHistory : UndoList Game
                newHistory =
                    history |> UndoList.new newGame
            in
            if (newGame.board |> Grid.emptyPositions |> (==) []) && mode /= Training then
                Action.transitioning
                    { game = newGame
                    , history = newHistory
                    , challenge = mode == Challenge
                    }

            else
                Action.updating
                    ( ( { state
                            | game = newGame
                            , history = newHistory
                            , collection = newCollection
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
            Action.updating
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
            Action.updating
                ( ( { state
                        | history = newHistory
                        , game = newHistory |> .present
                    }
                  , seed
                  )
                , Cmd.none
                )

        PageChangeRequested ->
            Action.updating
                ( ( { state
                        | viewCollection = not viewCollection
                        , viewedCard = Nothing
                    }
                  , seed
                  )
                , Cmd.none
                )

        CardSelected cellType ->
            Action.updating
                ( ( { state
                        | viewedCard = Just cellType
                    }
                  , seed
                  )
                , Cmd.none
                )



----------------------
-- View
----------------------


view :
    Float
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg msgMapper ( { game, selected, mode, viewCollection, collection, viewedCard }, _ ) =
    ( Nothing
    , [ if mode == Challenge then
            HeaderView.viewWithUndo scale
                { restartMsg = restartMsg
                , previousMsg = msgMapper Undo
                , nextMsg = msgMapper Redo
                }
                game.score

        else
            HeaderView.view scale
                restartMsg
                game.score
      , GameView.view
            { scale = scale
            , selected = selected
            , sort = mode /= Challenge
            }
            { positionSelectedMsg = msgMapper << PositionSelected
            , selectedMsg = msgMapper << Selected
            }
            game
      , (if viewCollection then
            PageSelectorView.viewCollection

         else
            PageSelectorView.viewGame
        )
        <|
            msgMapper PageChangeRequested
      ]
    )
