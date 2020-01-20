module FactoryCity.State.Playing exposing ( Model, Msg, TransitionData, init, update, view, subscriptions)

import Action
import Element exposing (Element)
import FactoryCity.Data.Board as Board
import FactoryCity.Data.CellType exposing (CellType)
import FactoryCity.Data.Deck as Deck exposing (Selected(..))
import FactoryCity.Data.Game as Game exposing (EndCondition(..), Game)
import FactoryCity.View.Game as GameView
import FactoryCity.View.Header as HeaderView
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import Http exposing (Error(..))
import Process
import Random exposing (Seed)
import Set exposing (Set)
import Task
import UndoList exposing (UndoList)
import Time


----------------------
-- Model
----------------------

type alias State =
    { game : Game
    , selected : Maybe Selected
    , viewedCard : Maybe CellType
    , initialSeed : Seed
    , running : Bool
    }


type alias Model =
    ( State, Seed )


type Msg
    = Selected Selected
    | PositionSelected Position
    | CardPlaced
    | CardSelected CellType
    | TimePassed


type alias TransitionData =
    { game : Game
    , seed : Seed
    }


type alias Action =
    Action.Action Model Msg Never ()



----------------------
-- Init
----------------------


init : TransitionData -> ( Model, Cmd Msg )
init { game, seed } =
    ( ( { game = game
        , selected = Nothing
        , running = False
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
play ( { game } as state, seed ) =
    let
        seconds : Float
        seconds =
            1000
    in
    Action.updating
        ( ( { state
                | game = game
                , selected = Nothing
            }
          , seed
          )
        , Task.perform (always CardPlaced) <| Process.sleep (0.1 * seconds)
        )


playFirst : Position -> Model -> Action
playFirst position ( { game, initialSeed } as state, seed ) =
    Random.step
        (Deck.playFirst { shuffle = True } game.deck
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
        (
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
update msg (( { game, selected } as state, seed ) as model) =
    let
        defaultCase : Action
        defaultCase =
            Action.updating
                ( model, Cmd.none )
    in
    case msg of
        TimePassed ->
            Action.updating
                    ( ( { state
                            | game = game |> Game.step
                        }
                      , seed
                      )
                    , Cmd.none
                    )

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
                Action.updating
                    ( ( { state
                            | game = game |> Game.step
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

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 (always TimePassed)

----------------------
-- View
----------------------


view :
    Float
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg msgMapper ( { game, selected, viewedCard }, _ ) =
    ( Nothing
    , [ HeaderView.view scale
                restartMsg
                game.score
      , GameView.view
            { scale = scale
            , selected = selected
            , sort = True
            }
            { positionSelectedMsg = msgMapper << PositionSelected
            , selectedMsg = msgMapper << Selected
            }
            game
      ]
    )