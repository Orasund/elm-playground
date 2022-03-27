module Zess.Main exposing (..)

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
import Time
import View.WrappedColumn exposing (Model)
import Zess.Config as Config
import Zess.Data.Figure as Figure exposing (Figure, FigureId)
import Zess.Data.Game as Game exposing (Change(..), Game)
import Zess.Data.Overlay exposing (Overlay(..))
import Zess.View.Board as Board
import Zess.View.Figure as Figure
import Zess.View.Overlay as Overlay


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


view : Model -> Document Msg
view model =
    { title = "Zess"
    , body =
        [ Html.node "meta"
            [ Attr.attribute "name" "viewport"
            , Attr.attribute "content" "width=device-width, initial-scale=1.0"
            ]
            []
        , Html.node "style"
            []
            [ ".button {border: 0px;}"
                ++ (".cell {border: solid 1px " ++ (Config.gray |> Color.toCssString) ++ "}")
                ++ "a:active {border: solid 1px black;}"
                |> Html.text
            ]
        , [ [ [ Html.text "Next Up"
              , model.game.next
                    |> Tuple.first
                    |> List.map (\figure -> figure |> Figure.toString False)
                    |> List.sort
                    |> List.map (Figure.view [ Attr.style "height" "20px" ])
                    |> Layout.row []
              ]
                |> Layout.column [ Layout.fill ]
            , model.game.score
                |> String.fromInt
                |> Html.text
                |> List.singleton
                |> Html.h1 [ Attr.style "text-align" "center", Attr.style "margin" "0" ]
            , Html.a
                [ Event.onClick (NewGame model.seed)
                , Attr.href "#"
                , Attr.style "text-decoration" "none"
                , Attr.style "color" "black"
                , Attr.style "font-size" "14px"
                , Attr.style "background-color" (Config.green |> Color.toCssString)
                , Attr.style "padding" "8px"
                , Attr.style "border-radius" "20px"
                , Attr.class "button"
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
                        Overlay.createCellOverlay
                            { game = model.game
                            , changes = model.changes
                            , gameOver = model.gameOver
                            }
                    , onClick = Click
                    , gameOver = model.gameOver
                    , score = model.game.score
                    , newGame = NewGame model.seed
                    }
            ]
                |> Layout.row [ Layout.spaceBetween ]
          ]
            |> Layout.column
                [ Layout.spacing (Config.boardSize / toFloat Config.size)
                , Attr.style "padding" "16px"
                , Attr.style "border" ("solid 1px " ++ Color.toCssString Color.black)
                , Attr.style "border-radius" "20px"
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
