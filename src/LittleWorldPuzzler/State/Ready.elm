module LittleWorldPuzzler.State.Ready exposing (Model, Msg, init, update, view)

import Element exposing (Element)
import Grid.Position exposing (Position)
import LittleWorldPuzzler.Data.Board as Board
import LittleWorldPuzzler.Data.Deck as Deck
import LittleWorldPuzzler.Data.Game as Game exposing (Game)
import LittleWorldPuzzler.State exposing (Action(..))
import LittleWorldPuzzler.View.Game as GameView
import LittleWorldPuzzler.View.Header as HeaderView
import Random exposing (Generator, Seed)
import UndoList exposing (UndoList)



----------------------
-- Model
----------------------


type alias State =
    { game : Game
    }


type alias Model =
    ( State, Seed )


type Msg
    = NormalModeSelected
    | TrainingModeSelected



----------------------
-- Init
----------------------


stateGenerator : Generator State
stateGenerator =
    Game.generator
        |> Random.map
            (\game ->
                { game = game
                }
            )


init : Seed -> ( Model, Cmd Msg )
init seed =
    ( Random.step stateGenerator seed, Cmd.none )



----------------------
-- Update
----------------------


update : Msg -> Model -> Action Model Msg { game : Game, seed : Seed, trainingMode : Bool }
update msg ( { game }, seed ) =
    case msg of
        NormalModeSelected ->
            Transition
                { game = game
                , seed = seed
                , trainingMode = False
                }

        TrainingModeSelected ->
            Transition
                { game = game
                , seed = seed
                , trainingMode = True
                }



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper ( { game }, _ ) =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ HeaderView.view scale restartMsg game.score
        , GameView.viewHome
            scale
            { normalModeSelectedMsg = msgMapper <| NormalModeSelected
            , trainingModeSelectedMsg = msgMapper <| TrainingModeSelected
            }
            game
        ]
