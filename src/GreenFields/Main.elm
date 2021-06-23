module GreenFields.Main exposing (main)

import Browser
import GreenFields.Page.InGame as InGame
import GreenFields.Page.Lobby as Lobby
import Html exposing (Html)


type Model
    = InGame InGame.Model
    | Lobby Lobby.Model


type Msg
    = WhileInGame InGame.Msg
    | WhileInLobby Lobby.Msg


init : () -> ( Model, Cmd Msg )
init () =
    Lobby.init ()
        |> Tuple.mapBoth Lobby (Cmd.map WhileInLobby)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WhileInGame mg, InGame ml ) ->
            InGame.update
                { modelMapper = InGame
                , restart = init ()
                , msgMapper = WhileInGame
                }
                mg
                ml

        ( WhileInLobby mg, Lobby ml ) ->
            Lobby.update
                { modelMapper = Lobby
                , upgrade = InGame.init >> InGame
                }
                mg
                ml
                |> Tuple.mapSecond (Cmd.map WhileInLobby)

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        InGame ml ->
            InGame.view ml
                |> Html.map WhileInGame

        Lobby ml ->
            Lobby.view ml |> Html.map WhileInLobby


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        InGame ml ->
            InGame.subscriptions ml
                |> Sub.map WhileInGame

        Lobby ml ->
            Lobby.subscriptions ml
                |> Sub.map WhileInLobby


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
