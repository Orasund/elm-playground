module Ruz.Main exposing (..)

import Browser exposing (Document)
import Color exposing (Color)
import Dict exposing (Dict)
import Element exposing (Attr)
import Emojidojo.String exposing (game)
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Layout
import List.Extra
import Random exposing (Seed)
import Ruz.Config as Config
import Ruz.Data.Figure as Figure exposing (Figure, FigureId)
import Ruz.Data.Game as Game exposing (Change(..), Game)
import Ruz.Data.Overlay exposing (Overlay(..))
import Ruz.View.Board as Board
import Time
import View.WrappedColumn exposing (Model)


type alias Model =
    { game : Game
    , gameOver : Bool
    , seed : Seed
    , changes : List Change
    , positions : Dict FigureId ( Int, Int )
    }


type Msg
    = Click ( Int, Int )
    | NewGame Seed
    | ApplyChange


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
        |> (\( ( game, changes ), seed ) ->
                { game = game
                , gameOver = False
                , seed = seed
                , changes = changes
                , positions = Dict.empty
                }
           )
    , Random.generate NewGame Random.independentSeed
    )


viewOverlay : { game : Game, changes : List Change, gameOver : Bool } -> Dict ( Int, Int ) Overlay
viewOverlay ({ game } as model) =
    if model.changes /= [] then
        Dict.empty

    else
        List.range 0 (Config.size - 1)
            |> List.concatMap
                (\i ->
                    List.range 0 (Config.size - 1)
                        |> List.map (\j -> ( i, j ))
                )
            |> List.filterMap
                (\pos ->
                    if model.gameOver then
                        Nothing

                    else if pos == model.game.player then
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
                            Just ( pos, Danger )

                        else if model.game |> Game.isDangerous pos then
                            Just ( pos, Warning )

                        else
                            Just ( pos, Success )

                    else
                        Nothing
                )
            |> Dict.fromList


view : Model -> Document Msg
view model =
    { title = "Ruz Puzzle"
    , body =
        [ Html.node "meta"
            [ Attr.attribute "name" "viewport"
            , Attr.attribute "content" "width=device-width, initial-scale=1.0"
            ]
            []
        , [ [ [ Html.text "Next Up"
              , model.game.next
                    |> Tuple.first
                    |> List.map (\figure -> figure |> Figure.toString False)
                    |> List.sort
                    |> List.map Html.text
                    |> Layout.row []
              ]
                |> Layout.column [ Layout.fill ]
            , model.game.score
                |> String.fromInt
                |> Html.text
                |> List.singleton
                |> Html.h1 [ Attr.style "text-align" "center", Attr.style "margin" "0" ]
            , Html.button
                [ Event.onClick (NewGame model.seed)
                , Attr.style "font-size" "14px"
                ]
                [ Html.text "New Game" ]
                |> Layout.el
                    [ Layout.fill
                    , Attr.style "align-items" "flex-start"
                    , Attr.style "justify-content" "flex-end"
                    ]
            ]
                |> Layout.row [ Attr.style "height" "40px" ]
          , [ model.positions
                |> Board.view
                    { figures = model.game.figures
                    , overlay =
                        viewOverlay
                            { game = model.game
                            , changes = model.changes
                            , gameOver = model.gameOver
                            }
                    , onClick = Click
                    , gameOver = model.gameOver
                    }
            ]
                |> Layout.row [ Layout.spaceBetween ]
          ]
            |> Layout.column
                [ Layout.spacing (Config.boardSize / toFloat Config.size)
                , Attr.style "padding" "8px"
                ]
            |> Layout.el
                [ Attr.style "position" "absolute"
                , Attr.style "left" "50%"
                , Attr.style "top" "50%"
                , Attr.style "transform" "translate(-50%, -50%)"
                ]
        ]
    }


applyChange : Change -> Model -> Model
applyChange change model =
    case change of
        Spawn figureId pos ->
            { model
                | positions =
                    model.positions
                        |> Dict.insert figureId pos
            }

        Move figureId pos ->
            { model
                | positions =
                    model.positions
                        |> Dict.update figureId
                            (Maybe.map (\_ -> pos))
            }

        Kill figureId ->
            { model
                | positions =
                    model.positions
                        |> Dict.remove figureId
            }

        GameOver ->
            { model
                | gameOver = True
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
                                    (\( game, changes ) ->
                                        { model
                                            | game = game
                                            , changes = changes
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
                |> (\( ( game, changes ), seed ) ->
                        { game = game
                        , gameOver = False
                        , seed = seed
                        , changes = changes
                        , positions = Dict.empty
                        }
                   )
            , Cmd.none
            )

        ApplyChange ->
            case model.changes of
                head :: tail ->
                    ( model
                        |> applyChange head
                        |> (\it -> { it | changes = tail })
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.changes of
        [] ->
            Sub.none

        _ ->
            Time.every 100 (always ApplyChange)
