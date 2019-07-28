module HighschoolDrama exposing (main)

import Action exposing (Action)
import Browser
import HighschoolDrama.Page.InGame as InGame
import HighschoolDrama.Page.Title as Title
import Html
import Random exposing (Seed)


type Model
    = Loading
    | Title Title.Model
    | InGame Game


type LoadingMsg
    = GotSeed Seed


type Msg
    = LoadingMsg LoadingMsg
    | TitleMsg Title.Msg
    | InGameMsg InGame.Msg


type alias LoadingAction =
    Action Never Never Seed Never


init : flag -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate (LoadingMsg << GotSeed) Random.independentSeed
    )


loadingUpdate : LoadingMsg -> () -> LoadingAction
loadingUpdate msg model =
    case msg of
        GotSeed seed ->
            Action.transitioning seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingMsg loadingMsg, Loading ) ->
            loadingUpdate loadingMsg ()
                |> Action.config
                |> Action.withTransition Title.init Title TitleMsg
                |> Action.apply

        ( TitleMsg titleMsg, Title titleModel ) ->
            Title.update titleMsg titleModel
                |> Action.config
                |> Action.withTransition InGame.init InGame InGameMsg
                |> Action.apply

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Highschool Drama Simulator"
    , body =
        case model of
            Loading ->
                []

            Title titleModel ->
                Title.view titleModel
                    |> List.map (Html.map TitleMsg)
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
