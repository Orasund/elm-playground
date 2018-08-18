module PixelEngine exposing (PixelEngine, program)

import Html.Styled as Html exposing (Html)
import PixelEngine.Controls as Controls exposing (Input)
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import Task
import Window


type alias PixelEngine flag model msg =
    Program flag (Model model) (Msg msg)


type alias Config =
    { windowSize : Maybe Window.Size
    , controls : Char -> Input
    }


type alias Model model =
    { modelContent : model
    , config : Config
    }


type Msg msg
    = Resize Window.Size
    | MsgContent msg


batch : ( model, Cmd msg ) -> Model model -> ( Model model, Cmd (Msg msg) )
batch ( modelContent, msg ) { config } =
    ( { modelContent = modelContent, config = config }, msg |> Cmd.map MsgContent )


updateFunction : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
updateFunction update msg ({ modelContent, config } as model) =
    case msg of
        Resize windowSize ->
            { model
                | config =
                    { config
                        | windowSize = Just windowSize
                    }
            }
                ! []

        MsgContent msg ->
            model |> batch (update msg modelContent)


subscriptionsFunction : (model -> Sub msg) -> Model model -> Sub (Msg msg)
subscriptionsFunction subscriptions ({ modelContent } as model) =
    Sub.batch
        [ subscriptions modelContent |> Sub.map MsgContent
        , Window.resizes Resize
        ]


viewFunction : (model -> ( Options msg, List (Area msg) )) -> Model model -> Html (Msg msg)
viewFunction view ({ modelContent, config } as model) =
    let
        ( options, listOfArea ) =
            view modelContent
    in
    (case config.windowSize of
        Just wS ->
            Graphics.render
                (options |> Controls.supportingMobile { windowSize = wS })
                listOfArea

        Nothing ->
            Graphics.render
                options
                []
    )
        |> Html.map MsgContent


initFunction : (model -> (Char -> Input)) -> ( model, Cmd msg ) -> ( Model model, Cmd (Msg msg) )
initFunction controls init =
    let
        ( modelContent, msg ) =
            init
    in
    { modelContent = modelContent
    , config = { windowSize = Nothing, controls = controls modelContent }
    }
        ! [ Task.perform Resize Window.size
          , msg |> Cmd.map MsgContent
          ]


programWithCustomControls :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> ( Options msg, List (Area msg) )
    , controls : model -> (Char -> Input)
    }
    -> Program Never (Model model) (Msg msg)
    
programWithCustomControls { init, update, subscriptions, view, controls } =
    Html.program
        { init =
            initFunction controls init
        , update =
            updateFunction update
        , subscriptions =
            subscriptionsFunction subscriptions
        , view =
            viewFunction view
        }



program :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> ( Options msg, List (Area msg) )
    }
    -> Program Never (Model model) (Msg msg)
program { init, update, subscriptions, view } =
    Html.program
        { init =
            initFunction Controls.defaultLayout init
        , update =
            updateFunction update
        , subscriptions =
            subscriptionsFunction subscriptions
        , view =
            viewFunction view
        }
