module Main exposing (main)

import Browser
import Dict
import Game exposing (Game)
import Html exposing (Html)
import Process
import Set
import String exposing (slice)
import Task
import View.Game


type alias Model =
    { game : Game
    , selected : Maybe ( Int, Int )
    }


type Msg
    = Select (Maybe ( Int, Int ))
    | RequestOpponentMove


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , selected = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select maybe ->
            case model.selected of
                Nothing ->
                    ( { model | selected = maybe }, Cmd.none )

                Just selected ->
                    case maybe of
                        Nothing ->
                            ( { model | selected = maybe }, Cmd.none )

                        Just to ->
                            ( model.game
                                |> Game.move { from = selected, to = to }
                                |> (\game ->
                                        { model
                                            | game = game
                                            , selected = Nothing
                                        }
                                   )
                            , Process.sleep 100
                                |> Task.perform (\() -> RequestOpponentMove)
                            )

        RequestOpponentMove ->
            model.game
                |> Game.findNextMove
                |> Maybe.map (\args -> Game.move args model.game)
                |> Maybe.withDefault model.game
                |> (\game -> ( { model | game = game }, Cmd.none ))


view : Model -> Html Msg
view model =
    View.Game.toHtml
        { selected = model.selected
        , onSelect = Select
        }
        model.game


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
