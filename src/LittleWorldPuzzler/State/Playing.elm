module LittleWorldPuzzler.State.Playing exposing (Model, Msg, init, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Framework.Modifier as Modifier exposing (Modifier(..))
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import Html exposing (Html)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry)
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.Request as Request exposing (Response(..))
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import LittleWorldPuzzler.View.Game as GameView
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
    , status : Maybe EndCondition
    , highscore : Maybe Entry
    }


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
                { game = game
                , selected = Nothing
                , history = UndoList.fresh game
                , status = Nothing
                , highscore = Nothing
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
        ( { game = game
          , selected = Nothing
          , history = history |> UndoList.new game
          , status = Nothing
          , highscore = Nothing
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
update replayGame modelMapper msg ( { game, highscore, history, selected } as state, seed ) =
    let
        defaultCase : ( model, Cmd Msg )
        defaultCase =
            ( modelMapper ( state, seed ), Cmd.none )
    in
    case msg of
        Selected select ->
            ( modelMapper ( { state | selected = Just select }, seed ), Cmd.none )

        PositionSelected position ->
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

        CardPlaced ->
            let
                newGame =
                    game |> Game.step

                status : Maybe EndCondition
                status =
                    if newGame.board |> Grid.emptyPositions |> (==) [] then
                        Just Lost

                    else
                        Nothing
            in
            ( modelMapper
                ( { state
                    | game = newGame
                    , history = history |> UndoList.new newGame
                    , status = status
                  }
                , seed
                )
            , case status of
                Just Lost ->
                    Request.getHighscore game.score
                        |> Cmd.map RequestedHighscore

                Just NewHighscore ->
                    Cmd.none

                Nothing ->
                    Cmd.none
            )

        RequestedReplay ->
            case highscore of
                Just h ->
                    case h.history |> UndoList.toList of
                        present :: future ->
                            ( replayGame <| UndoList.fromList present future
                            , Cmd.none
                            )

                        _ ->
                            defaultCase

                Nothing ->
                    defaultCase

        RequestedHighscore response ->
            case response of
                GotHighscore entry ->
                    ( modelMapper
                        ( { state
                            | highscore = Just entry
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
                        ( { state
                            | status = Just NewHighscore
                            , highscore = Just newEntry
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



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper ( { game, selected, status, highscore }, _ ) =
    let
        { board, deck, score } =
            game
    in
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.spaceEvenly
            , Element.centerX
            , Element.width <| Element.px <| floor <| 608 * scale
            ]
          <|
            [ Element.el
                [ Element.width <| Element.px <| floor <| 150 * scale
                ]
              <|
                Element.text ""
            , Element.el [ Font.size <| floor <| 50 * scale ] <|
                Element.text <|
                    String.fromInt score
            , Button.view
                [ Element.width <| Element.px <| floor <| 150 * scale
                , Element.padding <| floor <| 7 * scale
                , Border.rounded (floor <| 10 * scale)
                , Font.size <| floor <| 36 * scale
                , Font.family
                    [ Font.sansSerif ]
                ]
              <|
                { onPress = Just restartMsg
                , label = Element.text "Restart"
                }
            ]
        , GameView.view
            { scale = scale
            , selected = selected
            , restartMsg = restartMsg
            , status = status
            , highscore = highscore |> Maybe.map (.history >> .present >> .score)
            }
            (Just
                { positionSelectedMsg = msgMapper << PositionSelected
                , selectedMsg = msgMapper << Selected
                , requestedReplayMsg = msgMapper RequestedReplay
                }
            )
            game
        ]
