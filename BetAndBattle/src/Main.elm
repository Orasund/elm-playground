module Main exposing (..)

import Browser
import Card exposing (Card)
import Dict exposing (Dict)
import Html exposing (Html)


type alias Model =
    { yourCards : List Card
    , opponentCards : List Card
    , currentGoal : Maybe Card
    , playedCards : Dict Int Int
    }


type Msg
    = Play Card


init : () -> ( Model, Cmd Msg )
init () =
    ( { yourCards = []
      , opponentCards = []
      , currentGoal = Nothing
      , playedCards = Dict.empty
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.text ""


play : Card -> Model -> Model
play card model =
    { model
        | yourCards = model.yourCards
        , currentGoal = Just card
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play card ->
            ( play card model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
