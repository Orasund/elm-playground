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


type alias State =
    { game : Game
    , selected : Maybe Selected
    , history : UndoList Game
    }


type alias Model =
    ( State, Seed )


type Msg
    = Selected Selected
    | PositionSelected Position
    | CardPlaced



----------------------
-- Init
----------------------


stateGenerator : Generator State
stateGenerator =
    Game.generator
        |> Random.map
            (\game ->
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


play : (Msg -> msg) -> (Model -> model) -> UndoList Game -> ( Game, Seed ) -> ( model, Cmd msg )
play msgMapper modelMapper history ( game, seed ) =
    let
        seconds : Float
        seconds =
            1000
    in
    ( modelMapper
        ( { game = game
          , selected = Nothing
          , history = history |> UndoList.new game
          }
        , seed
        )
    , Task.perform (always <| msgMapper CardPlaced) <| Process.sleep (0.1 * seconds)
    )


playFirst : (Msg -> msg) -> (Model -> model) -> Position -> Game -> UndoList Game -> Seed -> ( model, Cmd msg )
playFirst msgMapper modelMapper position ({ board } as game) history seed =
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
        |> play msgMapper modelMapper history


playSecond : (Msg -> msg) -> (Model -> model) -> Position -> CellType -> Game -> UndoList Game -> (Seed -> ( model, Cmd msg ))
playSecond msgMapper modelMapper position cellType ({ board, deck } as game) history =
    { game
        | deck = deck |> Deck.playSecond
        , board = board |> Board.place position cellType
    }
        |> (\a b ->
                ( a, b ) |> play msgMapper modelMapper history
           )


update : (Game -> UndoList Game -> ( model, Cmd msg )) -> (Msg -> msg) -> (Model -> model) -> Msg -> Model -> ( model, Cmd msg )
update finished msgMapper modelMapper msg ( { game, history, selected } as model, seed ) =
    let
        defaultCase : ( model, Cmd msg )
        defaultCase =
            ( modelMapper ( model, seed ), Cmd.none )
    in
    case msg of
        Selected select ->
            ( modelMapper
                ( { model | selected = Just select }
                , seed
                )
            , Cmd.none
            )

        PositionSelected position ->
            case selected of
                Just First ->
                    playFirst msgMapper modelMapper position game history seed

                Just Second ->
                    case game.deck |> Deck.second of
                        Just second ->
                            playSecond msgMapper modelMapper position second game history seed

                        Nothing ->
                            playFirst msgMapper modelMapper position game history seed

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
            if newGame.board |> Grid.emptyPositions |> (==) [] then
                finished newGame newHistory

            else
                ( modelMapper
                    ( { model
                        | game = newGame
                        , history = newHistory
                      }
                    , seed
                    )
                , Cmd.none
                )



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper ( { game, selected }, _ ) =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ HeaderView.view scale restartMsg game.score
        , GameView.view
            { scale = scale
            , selected = selected
            }
            { positionSelectedMsg = msgMapper << PositionSelected
            , selectedMsg = msgMapper << Selected
            }
            game
        ]
