module GameJam exposing (main)

import Action
import Game exposing (Game)
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


subscriptions : Model -> Sub Running.Msg
subscriptions _ =
    Sub.none



{------------------------
   CONTROLS
------------------------}


controls : Input -> Maybe Running.Msg
controls input =
    case input of
        InputUp ->
            Just <| Running.Move Up

        InputDown ->
            Just <| Running.Move Down

        InputLeft ->
            Just <| Running.Move Left

        InputRight ->
            Just <| Running.Move Right

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
    { title = "One Switch"
    , options = Just options
    , body = areas model
    }


main : Game Running.Model Running.Msg
main =
    Game.main
        { init = Running.init
        , update = Running.update
        , subscriptions = subscriptions
        , view = Running.view
        , controls = controls
        , width = screenWidth
        }
