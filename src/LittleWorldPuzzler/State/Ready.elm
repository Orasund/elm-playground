module LittleWorldPuzzler.State.Ready exposing (Model, Msg, init, update, view)

import Action
import Element exposing (Element)
import LittleWorldPuzzler.Data.Game as Game exposing (Game)
import LittleWorldPuzzler.State.Playing as PlayingState exposing (Mode(..))
import LittleWorldPuzzler.View.Game as GameView
import LittleWorldPuzzler.View.Header as HeaderView
import LittleWorldPuzzler.View.PageSelector as PageSelectorView
import Random exposing (Generator, Seed)
import Task
import Time exposing (Month(..))



----------------------
-- Model
----------------------


type alias State =
    Game


type alias Model =
    ( State, Seed )


type Msg
    = NormalModeSelected
    | TrainingModeSelected
    | ChallengeModeSelected
    | ObtainedData ( Month, Int )


type alias Action =
    Action.Action Model Msg PlayingState.TransitionData Never



----------------------
-- Init
----------------------


stateGenerator : Generator State
stateGenerator =
    Game.generator


init : Seed -> ( Model, Cmd Msg )
init seed =
    ( Random.step stateGenerator seed, Cmd.none )



----------------------
-- Update
----------------------


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


update : Msg -> Model -> Action
update msg (( game, seed ) as model) =
    case msg of
        NormalModeSelected ->
            Action.transitioning
                { game = game
                , seed = seed
                , mode = Normal
                }

        TrainingModeSelected ->
            Action.transitioning
                { game = game
                , seed = seed
                , mode = Training
                }

        ChallengeModeSelected ->
            Action.updating
                ( model
                , Task.perform
                    (\t ->
                        ObtainedData ( t |> Time.toMonth Time.utc, t |> Time.toYear Time.utc )
                    )
                    Time.now
                )

        ObtainedData ( month, year ) ->
            let
                newSeed : Seed
                newSeed =
                    Random.initialSeed <| year * 100 + monthToInt month
            in
            Action.transitioning
                { game = Random.step Game.generator newSeed |> Tuple.first
                , seed = newSeed
                , mode = Challenge
                }



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper ( game, _ ) =
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
            , challengeModeSelectedMsg = msgMapper <| ChallengeModeSelected
            }
            game
        , PageSelectorView.viewInactive scale
        ]
