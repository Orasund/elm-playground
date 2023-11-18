module Main exposing (..)

import Browser
import Game exposing (Game)
import Html exposing (Html)
import View


type alias Model =
    { game : Game }


type Msg
    = Move Int


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.test }, Cmd.none )


view :
    Model
    ->
        { title : String
        , body : List (Html Msg)
        }
view model =
    { title = "Test"
    , body =
        View.toHtml Move
            model.game
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move int ->
            ( { game =
                    model.game
                        |> Game.move int
                        |> Maybe.withDefault model.game
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
