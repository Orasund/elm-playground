module Ruz.Main exposing (..)

import Browser exposing (Document)
import Color exposing (Color)
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
    , score : Int
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
        |> (\( game, seed ) ->
                { game = game
                , gameOver = False
                , seed = seed
                , score = 0
                }
           )
    , Random.generate NewGame Random.independentSeed
    )


view : Model -> Document Msg
view ({ game } as model) =
    let
        overlay : Dict ( Int, Int ) Color
        overlay =
            List.range 0 (Config.size - 1)
                |> List.concatMap
                    (\i ->
                        List.range 0 (Config.size - 1)
                            |> List.map (\j -> ( i, j ))
                    )
                |> List.filterMap
                    (\pos ->
                        if pos == model.game.player then
                            if model.gameOver then
                                Just ( pos, Config.red )

                            else
                                Nothing

                        else if model.gameOver then
                            Nothing

                        else if Game.valid { isEnemy = False, from = model.game.player, to = pos } model.game then
                            if
                                model.game.grid
                                    |> Dict.keys
                                    |> List.any
                                        (\enemyPos ->
                                            (enemyPos /= model.game.player)
                                                && (enemyPos /= pos)
                                                && Game.valid { isEnemy = True, from = enemyPos, to = pos }
                                                    { game
                                                        | grid =
                                                            game.grid
                                                                |> Dict.remove model.game.player
                                                                |> Dict.remove pos
                                                        , player = pos
                                                    }
                                        )
                            then
                                Just ( pos, Config.red )

                            else if game |> Game.isDangerous pos then
                                Just ( pos, Config.yellow )

                            else
                                Just ( pos, Config.green )

                        else
                            Nothing
                    )
                |> Dict.fromList
    in
    { title = "Ruz Puzzle"
    , body =
        [ [ model.score
                |> String.fromInt
                |> Html.text
                |> List.singleton
                |> Html.h1 [ Attr.style "text-align" "center", Attr.style "margin-bottom" "100px" ]
          , model.game.grid
                |> Board.view
                    { figures = model.game.figures
                    , player = model.game.player
                    , overlay = overlay
                    , onClick = Click
                    }
          ]
            |> Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "left" "50%"
                , Attr.style "top" "50%"
                , Attr.style "transform" "translate(-50%, -50%)"
                ]
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
                                        , score = model.score + 1
                                        }
                                    )
                                |> Maybe.withDefault model
                       )
            , Cmd.none
            )

        NewGame s ->
            ( s
                |> Random.step Game.init
                |> (\( game, seed ) -> { game = game, gameOver = False, seed = seed, score = 0 })
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
