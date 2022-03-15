module Ruz.Main exposing (..)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Layout
import Random exposing (Seed)
import Ruz.Config as Config
import Ruz.Data.Figure as Figure exposing (Figure)
import Ruz.Data.Game as Game exposing (Game)
import Ruz.View.Board as Board


type alias Model =
    { game : Game
    , gameOver : Bool
    , seed : Seed
    }


type Msg
    = Click ( Int, Int )
    | NewGame Seed


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
    ( Random.initialSeed 42
        |> Random.step Game.init
        |> (\( game, seed ) -> { game = game, gameOver = False, seed = seed })
    , Random.generate NewGame Random.independentSeed
    )


view : Model -> Document Msg
view model =
    { title = "Ruz Puzzle"
    , body =
        [ model.game.grid
            |> Board.view
                { figures = model.game.figures
                , player = model.game.player
                , gameOver = model.gameOver
                , onClick = Click
                }
            |> List.singleton
            |> Html.div
                [ Attr.style "margin" "flex"
                , Attr.style "justify-content" "center"
                , Attr.style "align-items" "center"
                ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            ( if model.gameOver then
                model

              else
                model.seed
                    |> Random.step (Game.move pos model.game)
                    |> (\( maybe, seed ) ->
                            maybe
                                |> Maybe.map
                                    (\{ gameOver, game } ->
                                        { game = game
                                        , gameOver = gameOver
                                        , seed = seed
                                        }
                                    )
                                |> Maybe.withDefault model
                       )
            , Cmd.none
            )

        NewGame s ->
            ( s
                |> Random.step Game.init
                |> (\( game, seed ) -> { game = game, gameOver = False, seed = seed })
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
