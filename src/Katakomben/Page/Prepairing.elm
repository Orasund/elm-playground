module Katakomben.Page.Prepairing exposing (Model, Msg, init, update, view)

import Action
import Browser.Dom as Dom
import Element exposing (Element)
import Katakomben.Page.Playing exposing (TansitionData)
import Random exposing (Seed)
import Task


type alias Model =
    Maybe Seed


type Msg
    = GotSeed Seed
    | Focused


type alias Action =
    Action.Action Model Never TansitionData Never


init : ( Model, Cmd Msg )
init =
    ( Nothing
    , Cmd.batch
        [ Random.generate GotSeed Random.independentSeed
        , Dom.focus "game-body"
            |> Task.attempt (always Focused)
        ]
    )


update : Msg -> Model -> Action
update msg model =
    case msg of
        GotSeed seed ->
            Action.transitioning seed

        Focused ->
            Action.updating ( model, Cmd.none )


view :
    Model
    -> List (Element msg)
view model =
    []
