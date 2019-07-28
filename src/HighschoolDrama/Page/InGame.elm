module HighschoolDrama.Page.InGame exposing (Action, Model, Msg(..), init, update, view)

import Action
import Element
import HighschoolDrama.Data exposing (Options)
import HighschoolDrama.Data.Game as Game exposing (Game)
import Html exposing (Html)
import Random exposing (Seed)


type alias Model =
    Game


type Msg
    = Idle


type alias Action =
    Action.Action Never Never Never Never


init : Options -> ( Model, Cmd Msg )
init options =
    ( Game.init options, Cmd Msg )


update : Msg -> Model -> Action
update msg model =
    case msg of
        StartGame maybeSex ->
            Action.transitioning maybeSex


view : Model -> List (Html Msg)
view model =
    List.singleton <|
        Element.layout [] <|
            Element.column [ Element.centerX ] <|
                [ Input.radio []
                    { onChange = StartGame
                    , selected = Nothing
                    , label = Input.labelAbove [] <| Element.text "Choose a Side:"
                    , options =
                        [ Input.option (Just Male) <| Element.text "Boys"
                        , Input.option (Just Male) <| Element.text "Girls"
                        , Input.option Nothing <| Element.text "Switch off mechanics based on sex"
                        ]
                    }
                ]
