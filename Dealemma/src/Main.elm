module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Game exposing (Card, Game)
import Game.Update
import Goal exposing (Goal)
import Html exposing (Html)
import Html.Attributes
import Html.Style as Style
import Layout
import Process
import Random exposing (Seed)
import Task
import View.Game
import View.Overlay


type Overlay
    = GameEnd


type alias Model =
    { game : Game
    , seed : Seed
    , overlay : Maybe Overlay
    , yourTurn : Bool
    }


type Msg
    = RestartGame Seed
    | PlayCard Card
    | ChallengeGoal
    | RequestOpponentTurn


init : () -> ( Model, Cmd Msg )
init () =
    ( restartGame (Random.initialSeed 42)
    , Random.independentSeed
        |> Random.generate RestartGame
    )


restartGame : Seed -> Model
restartGame seed =
    let
        ( game, newSeed ) =
            Game.fromGoals Goal.asList
                |> (\rand ->
                        Random.step rand seed
                   )
    in
    { game = game
    , seed = newSeed
    , overlay = Nothing
    , yourTurn = False
    }


view : Model -> Html Msg
view model =
    if model.overlay == Just GameEnd then
        View.Overlay.gameEnd
            { yourTurn = model.yourTurn }
            model.game

    else
        model.game
            |> View.Game.toHtml
                { onChallenge = ChallengeGoal
                , onPlay = PlayCard
                , yourTurn = model.yourTurn
                }
            |> Layout.el
                ([ Style.width "400px"
                 , Style.height "700px"
                 , Html.Attributes.style "padding" "8px"
                 , Style.boxSizingBorderBox
                 ]
                    ++ Layout.centered
                )


challengeGoal : Model -> Model
challengeGoal model =
    { model
        | overlay = Just GameEnd
    }


playCard : Card -> Model -> Model
playCard card model =
    model.game
        |> Game.Update.playCard card
        |> (\game ->
                { model
                    | game = game
                    , yourTurn = False
                }
           )


requestOpponentTurn : Model -> Model
requestOpponentTurn model =
    model.game
        |> Game.Update.opponentsTurn
        |> Maybe.map (\game -> { model | yourTurn = True, game = game })
        |> Maybe.withDefault { model | overlay = Just GameEnd }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RestartGame seed ->
            ( restartGame seed
            , Process.sleep 1000
                |> Task.perform (\() -> RequestOpponentTurn)
            )

        PlayCard card ->
            ( playCard card model
            , Process.sleep 1000
                |> Task.perform (\() -> RequestOpponentTurn)
            )

        ChallengeGoal ->
            ( challengeGoal model
            , Cmd.none
            )

        RequestOpponentTurn ->
            ( requestOpponentTurn model, Cmd.none )


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
