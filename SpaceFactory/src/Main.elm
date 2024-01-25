module Main exposing (main)

import Browser
import Game exposing (Game)
import Game.Update
import Html exposing (Html)
import Random exposing (Seed)
import Time
import View.Game


type alias Model =
    { game : Game
    , seed : Seed
    }


type Msg
    = UpdateRequested


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , seed = Random.initialSeed 42
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (\_ -> UpdateRequested)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRequested ->
            ( Random.step (Game.Update.tick model.game)
                model.seed
                |> (\( game, seed ) ->
                        { model
                            | game = game
                            , seed = seed
                        }
                   )
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    View.Game.toHtml model.game


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
