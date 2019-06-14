module AsteroidMiner.Main exposing (main)

import Action exposing (Action)
import AsteroidMiner.Data exposing (fps, size, spriteSize)
import AsteroidMiner.Data.Building exposing (BuildingType(..))
import AsteroidMiner.Data.Map exposing (SquareType(..))
import AsteroidMiner.Page.Game as Game
import Location exposing (Angle(..))
import PixelEngine exposing (Area, Input(..), PixelEngine, gameWithNoControls)
import PixelEngine.Options as Options exposing (Options)
import Random exposing (Seed)



----------------------
-- Model
----------------------


type Model
    = Loading
    | Game Game.Model


type LoadingMsg
    = GotSeed Seed


type Msg
    = GameSpecific Game.Msg
    | LoadingSpecific LoadingMsg


type alias LoadingAction =
    Action Never Never Seed Never



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.independentSeed |> Random.generate (LoadingSpecific << GotSeed)
    )



----------------------
-- Update
----------------------


updateLoading : LoadingMsg -> LoadingAction
updateLoading msg =
    case msg of
        GotSeed seed ->
            Action.transitioning seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingSpecific loadingMsg, Loading ) ->
            updateLoading loadingMsg
                |> Action.config
                |> Action.withTransition Game.init Game never
                |> Action.apply

        ( GameSpecific gameMsg, Game gameModel ) ->
            Game.update gameMsg gameModel
                |> Action.config
                |> Action.withExit (init ())
                |> Action.withUpdate Game never
                |> Action.apply

        _ ->
            ( model, Cmd.none )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Game gameModel ->
            gameModel
                |> Game.subscriptions
                |> Sub.map GameSpecific



----------------------
-- View
----------------------


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Tic Tac Toe"
    , options =
        Just
            (Options.default
                |> Options.withMovementSpeed (1 / fps)
            )
    , body =
        case model of
            Loading ->
                []

            Game gameModel ->
                Game.areas gameModel
                    |> List.map (PixelEngine.mapArea GameSpecific)
    }


height : Float
height =
    (toFloat <| size) * spriteSize


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , width = height
        }
