module LittleWorldPuzzler.State.Prepairing exposing (Model, Msg(..), update)

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
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import Process
import Random exposing (Generator, Seed)
import Task



----------------------
-- Model
----------------------


type alias Model =
    { scale : Maybe Float, seed : Maybe Seed }


type Msg
    = GotSeed Seed



----------------------
-- Update
----------------------


update :
    (Float -> Seed -> model)
    -> (Model -> model)
    -> Msg
    -> Model
    -> ( model, Cmd Msg )
update startPlaying modelMapper msg model =
    case msg of
        GotSeed seed ->
            case model.scale of
                Just scale ->
                    ( startPlaying scale seed
                    , Cmd.none
                    )

                Nothing ->
                    ( modelMapper { model | seed = Just seed }
                    , Cmd.none
                    )
