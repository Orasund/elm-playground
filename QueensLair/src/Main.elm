module Main exposing (main)

import Browser
import Dict
import Game exposing (Game)
import Game.Generate
import Html exposing (Html)
import Html.Attributes
import Layout
import Piece exposing (Piece(..))
import Process
import Random exposing (Seed)
import Task
import View.Game
import View.Shop


type alias Model =
    { game : Game
    , selected : Maybe ( Int, Int )
    , level : Int
    , party : List Piece
    , points : Int
    , openShop : Bool
    , seed : Seed
    }


type Msg
    = Select (Maybe ( Int, Int ))
    | RequestOpponentMove
    | GotSeed Seed
    | CloseShop
    | EndLevel
    | Recruit Piece


init : () -> ( Model, Cmd Msg )
init () =
    let
        lv =
            1

        party =
            [ King ]

        ( game, seed ) =
            Random.step
                (Game.Generate.generateByLevel lv
                    party
                )
                (Random.initialSeed 42)
    in
    ( { game = game
      , selected = Nothing
      , level = lv
      , party = party
      , points = 0
      , openShop = False
      , seed = seed
      }
    , Cmd.none
    )


startNextLevel : Model -> ( Model, Cmd Msg )
startNextLevel model =
    let
        level =
            model.level + 1

        ( game, seed ) =
            Random.step
                (Game.Generate.generateByLevel level
                    model.party
                )
                model.seed
    in
    ( { model
        | game = game
        , seed = seed
        , level = level
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
            (case model.game |> Game.findNextMove of
                Just args ->
                    { model | game = Game.move args model.game }

                Nothing ->
                    case model.game |> Game.possibleMoves { isYourTurn = True } of
                        head :: tail ->
                            Random.step (Random.uniform head tail)
                                model.seed
                                |> (\( move, seed ) ->
                                        { model
                                            | game = Game.move move model.game
                                            , seed = seed
                                        }
                                   )

                        [] ->
                            model
            )
                |> (\m -> ( m, Cmd.none ))

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        EndLevel ->
            ( { model
                | openShop = True
                , points = model.points + 1
                , party =
                    model.game.board
                        |> Dict.filter (\_ square -> square.isWhite)
                        |> Dict.toList
                        |> List.map (\( _, { piece } ) -> piece)
              }
            , Cmd.none
            )

        CloseShop ->
            startNextLevel { model | openShop = False }

        Recruit piece ->
            { model
                | openShop = False
                , party = piece :: model.party
                , points = model.points - Piece.value piece
            }
                |> startNextLevel


view : Model -> Html Msg
view model =
    (if model.openShop then
        View.Shop.toHtml
            { onLeave = CloseShop
            , points = model.points
            , onRecruit = Recruit
            }

     else
        [ View.Game.toHtml
            { selected = model.selected
            , onSelect = Select
            }
            model.game
        , if Game.isWon model.game then
            Layout.textButton []
                { label = "Next Level"
                , onPress = Just EndLevel
                }

          else
            Layout.none
        , (if Game.isLost model.game then
            "lost"

           else
            ""
          )
            |> Layout.text []
        ]
            |> Layout.column []
    )
        |> Layout.el [ Html.Attributes.style "width" "200px" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
