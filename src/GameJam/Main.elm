module GameJam exposing (main)

import Action
import GameJam.Data exposing (initialLevel, screenWidth)
import GameJam.Page.Running as Running
import Grid.Direction exposing (Direction(..))
import PixelEngine exposing (Area, Input(..), PixelEngine, game)
import PixelEngine.Options as Options exposing (Options)
import Random exposing (Seed)


type Model
    = Loading
    | Running Running.Model


type Msg
    = GotSeed Seed
    | RunningSpecific Running.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GotSeed Random.independentSeed
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            ( Running <| Running.init ( seed, initialLevel ), Cmd.none )

        ( RunningSpecific runningMsg, Running runningModel ) ->
            Running.update runningMsg runningModel
                |> Action.config
                |> Action.withUpdate Running RunningSpecific
                |> Action.withExit (init ())
                |> Action.apply

        _ ->
            ( model, Cmd.none )



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{------------------------
   CONTROLS
------------------------}


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| RunningSpecific <| Running.Move Up

        InputDown ->
            Just <| RunningSpecific <| Running.Move Down

        InputLeft ->
            Just <| RunningSpecific <| Running.Move Left

        InputRight ->
            Just <| RunningSpecific <| Running.Move Right

        _ ->
            Nothing



{------------------------
   VIEW
------------------------}


areas : Model -> List (Area Msg)
areas model =
    case model of
        Loading ->
            []

        Running runningModel ->
            Running.view runningModel
                |> List.map (PixelEngine.mapArea RunningSpecific)



{------------------------
   CONFIGURATION
------------------------}


options : Options Msg
options =
    Options.default


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "GMTK Game Jam 2019"
    , options = Just options
    , body = areas model
    }


main : PixelEngine () Model Msg
main =
    game
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = controls
        , width = screenWidth
        }
